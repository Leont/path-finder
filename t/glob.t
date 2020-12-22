use v6;
use Test;

use lib 't/lib';
use PFTest;

use Path::Finder;

sub glob($dir, |args) {
	return find($dir, |args, :relative, :map({ unixify( $^file, $dir ) }));
}

{
	my $dir = make-tree(<bar.md bar.txt foo.md foo.txt>);

	is-deeply(glob($dir, :relpath('*.md')), <bar.md foo.md>, ":relpath('*.md')");
	is-deeply(glob($dir, :relpath('foo.*')), <foo.md foo.txt>, ":relpath('foo.*')");
	is-deeply(glob($dir, :relpath('*')), <bar.md bar.txt foo.md foo.txt>, ":relpath('*')");
	is-deeply(glob($dir, :relpath('{foo,bar}.md')), <bar.md foo.md>, ":relpath('\{foo,bar}.md')");
	is-deeply(glob($dir, :relpath('[e-gz]*')), <foo.md foo.txt>, ":relpath('[a-gz]*')");
}

{
	my $dir = make-tree(<
		a/a/bar.md
		a/a/foo.md
		a/c/baz.md
		bar.md
		foo.md
		b/a/bar.md
		c/c/foo.md
	>);

	is-deeply(glob($dir, :relpath('*/*/foo.md')), <a/a/foo.md c/c/foo.md>, ":relpath('*/*/foo.md')");
	is-deeply(glob($dir, :relpath('*/*/bar.md')), <a/a/bar.md b/a/bar.md>, ":relpath('*/*/bar.md')");
	is-deeply(glob($dir, :relpath('*/*')), <a/a a/c b/a c/c>, ":relpath('*/*')");

	is-deeply(glob($dir, :relpath('**/foo.md')), <a/a/foo.md c/c/foo.md>, ":relpath('**/foo.md')");
}

done-testing;
