use v6;

unit class Path::Iterator;
has @.rules;
enum Prune <Prune-Inclusive Prune-Exclusive>;

my multi rulify(Callable $rule) {
	return $rule;
}
my multi rulify(Path::Iterator:D $rule) {
	return |$rule.rules;
}
multi method and(Path::Iterator:D $self: *@also) {
	return self.bless(:rules(|@!rules, |@also.map(&rulify)));
}
multi method and(Path::Iterator:U: *@also) {
	return self.bless(:rules(|@also.map(&rulify)));
}
method none(Path::Iterator:U: *@no) {
	return self.or(|@no).not;
}
method not() {
	my $obj = self;
	return self.bless(:rules[sub ($item, *%opts) {
		given $obj.test($item, |%opts) -> $original {
			when Prune {
				return Prune(+!$original);
			}
			default {
				return !$original;
			}
		}
	}]);
}
my multi unrulify(Callable $rule) {
	return Path::Iterator.bless(:rules[$rule]);
}
my multi unrulify(Path::Iterator:D $iterator) {
	return $iterator;
}
multi method or(Path::Iterator:U: $rule) {
	return unrulify($rule);
}
multi method or(Path::Iterator:U: *@also) {
	my @iterators = |@also.map(&unrulify);
	my @rules = sub ($item, *%opts) {
		my $ret = False;
		for @iterators -> $iterator {
			given $iterator.test($item, |%opts) {
				when Prune-Exclusive {
					$ret = $_;
				}
				when Prune-Inclusive {
					$ret = $_ if $ret === False;
				}
				when * === True {
					return True;
				}
			}
		}
		return $ret;
	}
	return self.bless(:@rules);
}
method skip(*@garbage) {
	my $obj = self.or(|@garbage);
	self.and(sub ($item, *%opts) {
		given $obj.test($item, |%opts) {
			when Prune {
				return Prune-Inclusive;
			}
			when * === True {
				return Prune-Inclusive;
			}
			default {
				return True;
			}
		}
	});
}

method test($item, *%args) {
	for @!rules -> &rule {
		unless rule($item, |%args) -> $value {
			return $value;
		}
	}
	return True;
}

method name(Mu $name) {
	self.and: sub ($item, *%) { $item.basename ~~ $name };
}
method dangling() {
	self.and: sub ($item, *%) { $item.l and not $item.e };
}
method not-dangling() {
	self.and: sub ($item, *%) { not $item.l or $item.e };
}

my $package = $?CLASS;
my %X-tests = %(
	:r('readable'),    :R('r-readable'),
	:w('writable'),    :W('r-writable'),
	:x('executable'),  :X('r-executable'),
	:o('owned'),       :O('r-owned'),

	:e('exists'),      :f('file'),
	:z('empty'),       :d('directory'),
	:s('nonempty'),    :l('symlink'),

	:u('setuid'),      :S('socket'),
	:g('setgid'),      :b('block'),
	:k('sticky'),      :c('character'),
	:p('fifo'),        :t('tty'),
);
for %X-tests.kv -> $test, $method {
	my $rule = sub ($item, *%) { ?$item."$test"() };
	$?CLASS.^add_method: $method, anon method () { return self.and($rule); };
	$?CLASS.^add_method: "not-$method", anon method () { return self.none($rule) };
}
$?CLASS.^compose;

method size($size) {
	self.and: sub ($item, *%) { $item.f && $item.s ~~ $size };
}
multi method depth(Range $depth-range) {
	self.and: sub ($item, :$depth, *%) {
		return do given $depth {
			when $depth-range.max {
				Prune-Exclusive;
			}
			when $depth-range {
				True;
			}
			when * < $depth-range.min {
				False;
			}
			default {
				Prune-Inclusive;
			}
		}
	};
}
multi method depth(Int $depth) {
	return self.depth($depth..$depth);
}

method skip-dir($pattern) {
	self.and: sub ($item, *%) {
		if $item.basename ~~ $pattern && $item.d {
			return Prune-Inclusive;
		}
		return True;
	}
}
method skip-subdirs($pattern) {
	self.and: sub ($item, :$depth, *%) {
		if $depth > 0 && $item.basename ~~ $pattern && $item.d {
			return Prune-Inclusive;
		}
		return True;
	}
}
method skip-hidden() {
	self.and: sub ($item, :$depth, *%) {
		if $depth > 0 && $item.basename ~~ rx/ ^ '.' / {
			return Prune-Inclusive;
		}
		return True;
	}
}
my @svn = $*DISTRO.name eq 'mswin32' ?? '_svn' !! ();
method skip-vcs() {
	return self.skip-dir(any(<.git .bzr .hg _darcs CVS RCS .svn>, |@svn)).name(none(rx/ '.#' $ /, rx/ ',v' $ /));
}

method shebang($pattern = rx/ ^ '#!' /, *%opts) {
	self.and: sub ($item, *%) {
		return False unless $item.f;
		return $item.lines(|%opts)[0] ~~ $pattern;
	}
}
method contents($pattern, *%opts) {
	self.and: sub ($item, *%) {
		return False unless $item.f;
		return $item.slurp(|%opts) ~~ $pattern;
	}
}
method line-match($pattern, *%opts) {
	self.and: sub ($item, *%) {
		return False unless $item.f;
		for $item.lines(|%opts) -> $line {
			return True if $line ~~ $pattern;
		}
		return False;
	}
}

my &is-unique = $*DISTRO.name ne any(<MSWin32 os2 dos NetWare symbian>)
	?? sub (Bool %seen, IO::Path $item) {
		use nqp;
		my $inode = nqp::p6box_i(nqp::stat(nqp::unbox_s(~$item), nqp::const::STAT_PLATFORM_INODE));
		my $device = nqp::p6box_i(nqp::stat(nqp::unbox_s(~$item), nqp::const::STAT_PLATFORM_DEV));
		my $key = "$inode\-$device";
		return False if %seen{$key};
		return %seen{$key} = True;
	}
	!! sub (Bool %seen, IO::Path $item) { return True };

method in(*@dirs,
	Bool :$follow-symlinks = True,
	Bool :$depth-first = False,
	Bool :$sorted = True,
	Bool :$loop-safe = True,
	Bool :$relative = False,
	:&visitor?,
) {
	@dirs = '.' if not @dirs;
	my @queue = @dirs.map: -> $filename {
		my $path = $filename.IO;
		($path, 0, $path, Bool);
	};

	gather {
		my Bool %seen;
		while @queue.elems {
			my ($item, $depth, $origin, $result) = @( @queue.shift );

			without ($result) {
				next if not $follow-symlinks and $item.l;

				$result = self.test($item, :$depth, :$origin);

				if &visitor && $result {
					visitor($item);
				}

				if $result !~~ Prune && $item.d && (!$loop-safe || is-unique(%seen, $item)) {
					my @next = $item.dir.map: -> $child { ($child, $depth + 1, $origin, Bool) };
					@next .= sort if $sorted;
					if ($depth-first) {
						@next.push: ($item, $depth, $origin, $result);
						@queue.prepend: @next;
						next;
					}
					else {
						@queue.append: @next;
					}
				}
			}

			take $relative ?? $item.relative($origin) !! $item if $result;
		}
	}
}
