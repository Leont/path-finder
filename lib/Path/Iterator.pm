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
	return self.bless(:rules[sub ($item, *%) {
		given $obj.test($item) -> $original {
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
	return Path::Iterator.new(:rules[$rule]);
}
my multi unrulify(Path::Iterator:D $iterator) {
	return $iterator;
}
multi method or(Path::Iterator:U: $rule) {
	return unrulify($rule);
}
multi method or(Path::Iterator:U: *@also) {
	my @iterators = |@also.map(&unrulify);
	my @rules = sub ($item, *%) {
		my $prune-inclusive = 0;
		for @iterators -> $iterator {
			given $iterator.test($item) {
				when Prune-Inclusive {
					$prune-inclusive++;
				}
				when * === True {
					return True;
				}
			}
		}
		return $prune-inclusive ?? Prune-Inclusive !! False;
	}
	return self.bless(:@rules);
}
method skip(*@garbage) {
	my $obj = self.new.or(|@garbage);
	self.and(sub ($item, *%) {
		given $obj.test($item) {
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
		return False if not rule($item, |%args);
	}
	return True;
}

method name($name) {
	self.and: sub ($item, *%) { $item.basename ~~ $name };
}
method dangling() {
	self.and: sub ($item, *%) { $item.l && !$item.e };
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

method shebang($pattern = rx/ ^ '#!' /) {
	self.and: sub ($item, *%) {
		return False unless $item.f;
		return $item.lines[0] ~~ $pattern;
	}
}
method contents($pattern) {
	self.and: sub ($item, *%) {
		return False unless $item.f;
		return $item.slurp ~~ $pattern;
	}
}
method line-match($pattern) {
	self.and: sub ($item, *%) {
		return False unless $item.f;
		for $item.lines -> $line {
			return True if $line ~~ $pattern;
		}
		return False;
	}
}

method !is-unique(IO::Path $item, *%opts) {
	return True; # XXX
}
method in(*@dirs,
	Bool :$follow-symlinks = True,
	Bool :$depth-first = False,
	Bool :$sorted = True,
	Bool :$loop-safe = True,
	Bool :$relative = False,
	:&visitor?,
	:&error-handler = sub ($item, $reason) { die sprintf "%s: %s\n", $item, $reason }
) {
	@dirs = '.' if not @dirs;
	my @queue = @dirs.map: -> $filename {
		my $path = $filename.IO;
		($path, 0, $path, Bool);
	};

	gather {
		while @queue.elems {
			my ($item, $depth, $origin, $result) = @( @queue.shift );

			without ($result) {
				next if not $follow-symlinks and $item.l;

				$result = @!rules ?? self.test($item, :$depth, :$origin) !! True;

				if &visitor && $result {
					visitor($item);
				}

				if $result !~~ Prune && $item.d && (!$loop-safe || self!is-unique($item)) {
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
