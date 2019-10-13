unit package PFTest;

use File::Temp;

sub make-tree(@files) is export(:DEFAULT, :make-tree) {
    my $td = tempdir;
    for (@files) -> $file {
		my $path = $td.IO.add($file);
        if $file.ends-with('/') {
			$path.mkdir;
        }
        else {
			$path.parent.mkdir;
			$path.spurt('');
        }
    }
    return $td.IO;
}

sub unixify(IO::Path $path, IO::Path $dir) is export(:DEFAULT, :unixify) {
    my $relative = $path.relative($dir);
	return IO::Spec::Unix.catdir($*SPEC.splitdir($relative));
}
