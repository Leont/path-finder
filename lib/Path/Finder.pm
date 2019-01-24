use v6;

unit class Path::Finder;

use IO::Glob;

has Callable:D @!rules;
our enum Prune is export(:prune) <PruneInclusive PruneExclusive>;

submethod BUILD(:@!rules) { }
method !rules() {
	return @!rules;
}

our enum Precedence is export(:traits) <Skip Depth Name Stat Content And None Or Not>;

our role Constraint {
	has Precedence:D $.precedence is required;
}

multi sub trait_mod:<is>(Method $method, Precedence:D :$constraint!) is export(:traits) {
	trait_mod:<of>($method, Path::Finder:D);
	return $method does Constraint($constraint);
}

my multi rulify(Callable $rule) {
	return $rule;
}
my multi rulify(Path::Finder:D $rule) {
	return $rule!rules;
}
proto method and(*@) is constraint(And) { * }
multi method and(Path::Finder:D $self: *@also) {
	return self.bless(:rules(|@!rules, |@also.map(&rulify)));
}
multi method and(Path::Finder:U: *@also) {
	return self.bless(:rules(|@also.map(&rulify)));
}
proto method none(|) is constraint(None) { * }
multi method none(Path::Finder:U: *@no) {
	return self.or(|@no).not;
}
multi method none(Path::Finder:D: Callable $rule) {
	return self.and: sub ($item, *%options) { return negate($rule($item, |%options)) };
}

my multi negate(Bool $value) {
	return !$value;
}
my multi negate(Prune $value) {
	return Prune(+!$value)
}
method not() {
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
proto method or(*@) is constraint(Or) { * }
multi method or(Path::Finder:U: $rule) {
	return unrulify($rule);
}
multi method or(Path::Finder:U: *@also)  {
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
method skip(*@garbage) is constraint(Skip) {
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

method name(Mu $name) is constraint(Name) {
	my $matcher = globulize($name);
	self.and: sub ($item, *%) { $item.basename ~~ $matcher };
}

method ext(Mu $ext) is constraint(Name) {
	self.and: sub ($item, *%) { $item.extension ~~ $ext };
}

method path(Mu $path) is constraint(Name) {
	my $matcher = globulize($path);
	self.and: sub ($item, *%) { ~$item ~~ $matcher };
}

method relpath(Mu $path ) is constraint(Name) {
	my $matcher = globulize($path);
	self.and: sub ($item, :$base, *%) { $item.relative($base) ~~ $matcher };
}

method io(Mu $path) is constraint(Name) {
	self.and: sub ($item, *%) { $item ~~ $path };
}

method dangling(Bool $value = True) is constraint(Stat) {
	self.and: sub ($item, *%) { ($item.l && !$item.e) == $value };
}

method readable(Bool $value = True) is constraint(Stat) {
	self.and: sub ($item, *%) { $item.r == $value };
}
method writable(Bool $value = True) is constraint(Stat) {
	self.and: sub ($item, *%) { $item.w == $value };
}
method executable(Bool $value = True) is constraint(Stat) {
	self.and: sub ($item, *%) { $item.x == $value };
}
method read-writable(Bool $value = True) is constraint(Stat) {
	self.and: sub ($item, *%) { $item.rw == $value };
}
method read-write-executable(Bool $value = True) is constraint(Stat) {
	self.and: sub ($item, *%) { $item.rwx == $value };
}
method exists(Bool $value = True) is constraint(Stat) {
	self.and: sub ($item, *%) { $item.e == $value };
}
method file(Bool $value = True) is constraint(Stat) {
	self.and: sub ($item, *%) { $item.f == $value };
}
method directory(Bool $value = True) is constraint(Stat) {
	self.and: sub ($item, *%) { $item.d == $value };
}
method symlink(Bool $value = True) is constraint(Stat) {
	self.and: sub ($item, *%) { $item.l == $value }
}
method empty(Bool $value = True) is constraint(Stat) {
	self.and: sub ($item, *%) { $item.z == $value };
}

{
	use nqp;
	method inode(Mu $inode) is constraint(Stat) {
		self.and: sub ($item, *%) { nqp::stat(nqp::unbox_s(~$item), nqp::const::STAT_PLATFORM_INODE) ~~ $inode};
	}
	method device(Mu $device) is constraint(Stat) {
		self.and: sub ($item, *%) { nqp::stat(nqp::unbox_s(~$item), nqp::const::STAT_PLATFORM_DEV) ~~ $device };
	}
	method nlinks(Mu $nlinks) is constraint(Stat) {
		self.and: sub ($item, *%) { nqp::stat(nqp::unbox_s(~$item), nqp::const::STAT_PLATFORM_NLINKS) ~~ $nlinks };
	}
	method uid(Mu $uid) is constraint(Stat) {
		self.and: sub ($item, *%) { nqp::stat(nqp::unbox_s(~$item), nqp::const::STAT_UID) ~~ $uid };
	}
	method gid(Mu $gid) is constraint(Stat) {
		self.and: sub ($item, *%) { nqp::stat(nqp::unbox_s(~$item), nqp::const::STAT_GID) ~~ $gid };
	}
}

method accessed(Mu $accessed) is constraint(Stat) {
	self.and: sub ($item, *%) { $item.accessed ~~ $accessed };
}
method changed(Mu $changed) is constraint(Stat) {
	self.and: sub ($item, *%) { $item.changed ~~ $changed };
}
method modified(Mu $modified) is constraint(Stat) {
	self.and: sub ($item, *%) { $item.modified ~~ $modified };
}

method mode(Mu $mode) is constraint(Stat) {
	self.and: sub ($item, *%) { $item.mode ~~ $mode };
}
method size(Mu $size) is constraint(Stat) {
	self.and: sub ($item, *%) { $item.s ~~ $size };
}

proto method depth($) is constraint(Depth) { * }
multi method depth(Range $depth-range where .is-int) {
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
multi method depth(Int $depth) {
	return self.depth($depth..$depth);
}
multi method depth(Mu $depth-match) {
	self.and: sub ($item, :$depth, *%) {
		return $depth ~~ $depth-match;
	}
}

method skip-dir(Mu $pattern) is constraint(Skip) {
	self.and: sub ($item, *%) {
		if $item.basename ~~ $pattern && $item.d {
			return PruneInclusive;
		}
		return True;
	}
}
method skip-subdir(Mu $pattern) is constraint(Skip) {
	self.and: sub ($item, :$depth, *%) {
		if $depth > 0 && $item.basename ~~ $pattern && $item.d {
			return PruneInclusive;
		}
		return True;
	}
}
method skip-hidden(Bool $hide = True) is constraint(Skip) {
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
method skip-vcs(Bool $hide = True) is constraint(Skip) {
	self.skip-dir($vcs-dirs).name($vcs-files) if $hide;
}

proto method shebang(Mu $pattern, *%opts) is constraint(Content) { * }
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

method contents(Mu $pattern, *%opts) is constraint(Content) {
	self.and: sub ($item, *%) {
		return False unless $item.f;
		return $item.slurp(|%opts) ~~ $pattern;
	};
}
method lines(Mu $pattern, *%opts) is constraint(Content) {
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

our sub finder(Path::Finder :$base = Path::Finder, *%options --> Path::Finder) is export(:find) {
	class Entry {
		has $.name;
		has $.method handles <precedence>;
		has $.capture;
		method call-with($object) {
			return $object.$!method(|$!capture);
		}
	};
	my Entry @entries;
	for %options.kv -> $name, $value {
		my $method = $base.^lookup($name);
		die "Finder key $name invalid" if not $method.defined or $method !~~ Constraint;
		my $capture = $value ~~ Capture ?? $value !! do given $method.signature.count - 1 {
			when 1 {
				\($value);
			}
			when Inf {
				\(|@($value).map: -> $entry { $entry ~~ Hash|Pair ?? finder(|%($entry)) !! $entry });
			}
		}
		@entries.push: Entry.new(:$name, :$method, :$capture);
	}
	my @keys = @entries.sort(*.precedence);
	return ($base, |@keys).reduce: -> $object, $entry {
		$entry.call-with($object);
	}
}

our sub find(*@dirs, *%options --> Seq:D) is export(:DEFAULT :find) {
	my %in-options = %options<follow-symlinks order sorted loop-safe relative as map>:delete:p;
	return finder(|%options).in(|@dirs, |%in-options);
}
