#!/usr/bin/perl -w

$HOME=$ENV{'HOME'};

while ( $s = <> )
{
    $s =~ s/$HOME\/NFS\/WWW\///gi;
    $s =~ s/$HOME\/WWW\///gi;
    print $s;
};






