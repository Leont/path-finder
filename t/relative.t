use v6;
use Test;

use lib 't/lib';
use PFTest :make-tree;

use Path::Finder;

sub unixify($input) {
	return IO::Spec::Unix.catdir($*SPEC.splitdir($input));
}

#--------------------------------------------------------------------------#

{
	my @tree = <
	  aaaa.txt
	  bbbb.txt
	  cccc/dddd.txt
	  cccc/eeee/ffff.txt
	  gggg.txt
	>;

	my @depth_pre = <
	  .
	  aaaa.txt
	  bbbb.txt
	  cccc
	  cccc/dddd.txt
	  cccc/eeee
	  cccc/eeee/ffff.txt
	  gggg.txt
	>;

	my @depth_post = <
	  aaaa.txt
	  bbbb.txt
	  cccc/dddd.txt
	  cccc/eeee/ffff.txt
	  cccc/eeee
	  cccc
	  gggg.txt
	  .
	>;

	my $td = make-tree(@tree);

	my $rule = Path::Finder.new;

	my @files = $rule.in($td, :order(PreOrder), :relative, :map({ unixify($_.IO) }));
	is-deeply(@files, @depth_pre, "Depth first iteration (pre)");

	my @files2 = $rule.in($td, :order(PostOrder), :relative, :map({ unixify($_.IO) }));
	is-deeply(@files2, @depth_post, "Depth first iteration (post)");

}

done-testing;

# This file is derived from Path-Iterator-Rule, Copyright (c) 2013 by David Golden.
