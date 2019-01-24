use v6;

unit class Path::Finder;

use IO::Glob;

has Callable:D @!rules;
our enum Prune is export(:prune) <PruneInclusive PruneExclusive>;

submethod BUILD(:@!rules) { }
method !rules() {
	return @!rules;
}

my multi rulify(Callable $rule) {
	return $rule;
}
my multi rulify(Path::Finder:D $rule) {
	return $rule!rules;
}
proto method and(*@ --> Path::Finder:D) { * }
multi method and(Path::Finder:D $self: *@also --> Path::Finder:D) {
	return self.bless(:rules(|@!rules, |@also.map(&rulify)));
}
multi method and(Path::Finder:U: *@also --> Path::Finder:D) {
	return self.bless(:rules(|@also.map(&rulify)));
}
multi method none(Path::Finder:U: *@no --> Path::Finder:D) {
	return self.or(|@no).not;
}
multi method none(Path::Finder: Callable $rule --> Path::Finder:D) {
	return self.and: sub ($item, *%options) { return negate($rule($item, |%options)) };
}

my multi negate(Bool $value) {
	return !$value;
}
my multi negate(Prune $value) {
	return Prune(+!$value)
}
method not(--> Path::Finder:D) {
	my $obj = self;
	return self.bless(:rules[sub ($item, *%opts) {
		return negate($obj!test($item, |%opts))
	}]);
}
my multi unrulify(Callable $rule) {
	return Path::Finder.and($rule);
}
my multi unrulify(Path::Finder $iterator) {
	return $iterator;
}
proto method or(*@ --> Path::Finder:D) { * }
multi method or(Path::Finder:U: $rule --> Path::Finder:D) {
	return unrulify($rule);
}
multi method or(Path::Finder:U: *@also --> Path::Finder:D) {
	my @iterators = |@also.map(&unrulify);
	my @rules = sub ($item, *%opts) {
		my $ret = False;
		for @iterators -> $iterator {
			given $iterator!test($item, |%opts) {
				when * === True {
					return True;
				}
				when PruneExclusive {
					$ret = $_;
				}
				when PruneInclusive {
					$ret = $_ if $ret === False;
				}
			}
		}
		return $ret;
	}
	return self.bless(:@rules);
}
method skip(*@garbage --> Path::Finder:D) {
	my @iterators = |@garbage.map(&unrulify);
	self.and: sub ($item, *%opts) {
		for @iterators -> $iterator {
			if $iterator!test($item, |%opts) !== False {
				return PruneInclusive;
			}
		}
		return True;
	};
}

method !test(IO::Path $item, *%args) {
	my $ret = True;
	for @!rules -> &rule {
		my $value = rule($item, |%args);
		return $value unless $value;
		$ret = $value if $value === PruneExclusive;
	}
	return $ret;
}

my multi sub globulize(Any $name) {
	return $name;
}
my multi sub globulize(Str $name) {
	return glob($name);
}

method name(Mu $name --> Path::Iterator:D) {
	my $matcher = globulize($name);
	self.and: sub ($item, *%) { $item.basename ~~ $matcher };
}

method ext(Mu $ext --> Path::Finder:D) {
	self.and: sub ($item, *%) { $item.extension ~~ $ext };
}

method path(Mu $path --> Path::Iterator:D) {
	my $matcher = globulize($path);
	self.and: sub ($item, *%) { ~$item ~~ $matcher };
}

method relpath(Mu $path --> Path::Iterator:D) {
	my $matcher = globulize($path);
	self.and: sub ($item, :$base, *%) { $item.relative($base) ~~ $matcher };
}

method io(Mu $path --> Path::Finder:D) {
	self.and: sub ($item, *%) { $item ~~ $path };
}

method dangling(Bool $value = True --> Path::Finder:D) {
	self.and: sub ($item, *%) { ($item.l && !$item.e) == $value };
}

method readable(Bool $value = True --> Path::Finder:D) {
	self.and: sub ($item, *%) { $item.r == $value };
}
method writable(Bool $value = True --> Path::Finder:D) {
	self.and: sub ($item, *%) { $item.w == $value };
}
method executable(Bool $value = True --> Path::Finder:D) {
	self.and: sub ($item, *%) { $item.x == $value };
}
method read-writable(Bool $value = True --> Path::Finder:D) {
	self.and: sub ($item, *%) { $item.rw == $value };
}
method read-write-executable(Bool $value = True --> Path::Finder:D) {
	self.and: sub ($item, *%) { $item.rwx == $value };
}
method exists(Bool $value = True --> Path::Finder:D) {
	self.and: sub ($item, *%) { $item.e == $value };
}
method file(Bool $value = True --> Path::Finder:D) {
	self.and: sub ($item, *%) { $item.f == $value };
}
method directory(Bool $value = True --> Path::Finder:D) {
	self.and: sub ($item, *%) { $item.d == $value };
}
method symlink(Bool $value = True --> Path::Finder:D) {
	self.and: sub ($item, *%) { $item.l == $value }
}
method empty(Bool $value = True --> Path::Finder:D) {
	self.and: sub ($item, *%) { $item.z == $value };
}

{
	use nqp;
	method inode(Mu $inode --> Path::Finder:D) {
		self.and: sub ($item, *%) { nqp::stat(nqp::unbox_s(~$item), nqp::const::STAT_PLATFORM_INODE) ~~ $inode};
	}
	method device(Mu $device --> Path::Finder:D) {
		self.and: sub ($item, *%) { nqp::stat(nqp::unbox_s(~$item), nqp::const::STAT_PLATFORM_DEV) ~~ $device };
	}
	method nlinks(Mu $nlinks --> Path::Finder:D) {
		self.and: sub ($item, *%) { nqp::stat(nqp::unbox_s(~$item), nqp::const::STAT_PLATFORM_NLINKS) ~~ $nlinks };
	}
	method uid(Mu $uid --> Path::Finder:D) {
		self.and: sub ($item, *%) { nqp::stat(nqp::unbox_s(~$item), nqp::const::STAT_UID) ~~ $uid };
	}
	method gid(Mu $gid --> Path::Finder:D) {
		self.and: sub ($item, *%) { nqp::stat(nqp::unbox_s(~$item), nqp::const::STAT_GID) ~~ $gid };
	}
}

method accessed(Mu $accessed --> Path::Finder:D) {
	self.and: sub ($item, *%) { $item.accessed ~~ $accessed };
}
method changed(Mu $changed --> Path::Finder:D) {
	self.and: sub ($item, *%) { $item.changed ~~ $changed };
}
method modified(Mu $modified--> Path::Finder:D) {
	self.and: sub ($item, *%) { $item.modified ~~ $modified };
}

method mode(Mu $mode--> Path::Finder:D) {
	self.and: sub ($item, *%) { $item.mode ~~ $mode };
}
method size(Mu $size --> Path::Finder:D) {
	self.and: sub ($item, *%) { $item.s ~~ $size };
}

proto method depth($ --> Path::Finder:D) { * }
multi method depth(Range $depth-range where .is-int --> Path::Finder:D) {
	my ($min, $max) = $depth-range.int-bounds;
	self.and: sub ($item, :$depth, *%) {
		return do given $depth {
			when $max {
				PruneExclusive;
			}
			when $depth-range {
				True;
			}
			when * < $min {
				False;
			}
			default {
				PruneInclusive;
			}
		}
	};
}
multi method depth(Int $depth --> Path::Finder:D) {
	return self.depth($depth..$depth);
}
multi method depth(Mu $depth-match --> Path::Finder:D) {
	self.and: sub ($item, :$depth, *%) {
		return $depth ~~ $depth-match;
	}
}

method skip-dir(Mu $pattern --> Path::Finder:D) {
	self.and: sub ($item, *%) {
		if $item.basename ~~ $pattern && $item.d {
			return PruneInclusive;
		}
		return True;
	}
}
method skip-subdir(Mu $pattern --> Path::Finder:D) {
	self.and: sub ($item, :$depth, *%) {
		if $depth > 0 && $item.basename ~~ $pattern && $item.d {
			return PruneInclusive;
		}
		return True;
	}
}
method skip-hidden(Bool $hide = True --> Path::Finder:D) {
	if $hide {
		self.and: sub ($item, :$depth, *%) {
			if $depth > 0 && $item.basename ~~ rx/ ^ '.' / {
				return PruneInclusive;
			}
			return True;
		}
	}
}
my $vcs-dirs = any(<.git .bzr .hg _darcs CVS RCS .svn>, |($*DISTRO.name eq 'mswin32' ?? '_svn' !! ()));
my $vcs-files = none(rx/ '.#' $ /, rx/ ',v' $ /);
method skip-vcs(Bool $hide = True --> Path::Finder:D) {
	self.skip-dir($vcs-dirs).name($vcs-files) if $hide;
}

proto method shebang(Mu $pattern, *%opts --> Path::Finder:D) { * }
multi method shebang(Mu $pattern, *%opts) {
	self.and: sub ($item, *%) {
		return False unless $item.f;
		return $item.lines(|%opts)[0] ~~ $pattern;
	};
}
multi method shebang(Bool $value = True, *%opts) {
	self.and: sub ($item, *%) {
		return !$value unless $item.f;
		return ?($item.lines(|%opts)[0] ~~ rx/ ^ '#!' /) === $value;
	};
}

method contents(Mu $pattern, *%opts --> Path::Finder:D) {
	self.and: sub ($item, *%) {
		return False unless $item.f;
		return $item.slurp(|%opts) ~~ $pattern;
	};
}
method lines(Mu $pattern, *%opts --> Path::Finder:D) {
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
		my $inode = nqp::stat(nqp::unbox_s(~$item), nqp::const::STAT_PLATFORM_INODE);
		my $device = nqp::stat(nqp::unbox_s(~$item), nqp::const::STAT_PLATFORM_DEV);
		my $key = "$inode\-$device";
		return False if %seen{$key};
		return %seen{$key} = True;
	}
	!! sub (Bool %seen, IO::Path $item) { return True };

enum Order is export(:DEFAULT :order) < BreadthFirst PreOrder PostOrder >;

my %as{Any:U} = ((Str) => { ~$_ }, (IO::Path) => Block);
multi method in(Path::Finder:D:
	*@dirs,
	Bool:D :$follow-symlinks = True,
	Bool:D :$report-symlinks = $follow-symlinks,
	Order:D :$order = BreadthFirst,
	Bool:D :$sorted = True,
	Bool:D :$loop-safe = True,
	Bool:D :$relative = False,
	Any:U :$as = IO::Path,
	:&map = %as{$as},
	--> Seq:D
) {
	my @queue = (@dirs || '.').map(*.IO).map: { ($^path, 0, $^path, Bool) };

	my Bool $check-symlinks = !$follow-symlinks || !$report-symlinks;
	my Bool %seen;
	my $seq := gather while @queue {
		my ($item, $depth, $base, $result) = @( @queue.shift );

		without $result {
			my $is-link = $check-symlinks ?? $item.l !! False;
			next if $is-link && !$report-symlinks;

			$result = self!test($item, :$depth, :$base);
			my $prune = $result ~~ Prune || $is-link && !$follow-symlinks;

			if !$prune && $item.d && (!$loop-safe || is-unique(%seen, $item)) {
				my @next = $item.dir.map: { ($^child, $depth + 1, $base, Bool) };
				@next .= sort if $sorted;
				given $order {
					when BreadthFirst {
						@queue.append: @next;
					}
					when PostOrder {
						@next.push: ($item, $depth, $base, $result);
						@queue.prepend: @next;
						next;
					}
					when PreOrder {
						@queue.prepend: @next;
					}
				}
			}
		}

		take $relative ?? $item.relative($base).IO !! $item if $result;
	}
	return &map ?? $seq.map(&map) !! $seq;
}

multi method in(Path::Finder:U: |args --> Seq:D){
	return self.new.in(|args);
}

my %priority = (
	skip        => 0,
	skip-hidden => 0,
	skip-dir    => 0,
	skip-subdir => 0,
	skip-vcs    => 0,
	depth       => 1,
	name        => 2,
	ext         => 2,
	path        => 2,
	relpath     => 2,
	io          => 2,
	# default priority is 3
	contents    => 4,
	lines       => 4,
	shebang     => 4,
	not         => 5,
);

our sub finder(Path::Finder :$base = Path::Finder, *%options --> Path::Finder) is export(:find) {
	my @keys = %options.keys.sort: { %priority{$_} // 3 };
	return ($base, |@keys).reduce: -> $object, $name {
		my $method = $object.^lookup($name);
		die "Finder key $name invalid" if not $method.defined or $method.signature.returns !~~ Path::Finder;
		my $value = %options{$name};
		my $capture = $value ~~ Capture ?? $value !! do given $method.signature.count - 1 {
			when 1 {
				\($value);
			}
			when Inf {
				\(|@($value).map: -> $entry { $entry ~~ Hash|Pair ?? finder(|%($entry)) !! $entry });
			}
		}
		$object.$method(|$capture);
	}
}

our sub find(*@dirs, *%options --> Seq:D) is export(:DEFAULT :find) {
	my %in-options = %options<follow-symlinks order sorted loop-safe relative as map>:delete:p;
	return finder(|%options).in(|@dirs, |%in-options);
}
