#!/usr/bin/perl -w
use strict;
use File::Copy;
use File::stat;
use Time::localtime;

myInit();
myRun();
myDone();



sub myInit
{
  print ("  <table width='*' border='0'>\n");
};

sub myDone
{
  print ("\n  </table>\n\n");
};

sub myRun
{
  my $basePath = ".";	### from the current directory
  myWalk("$basePath");
};


sub myWalk
{

  opendir( DIR, "$_[0]" );
#  my $a; my $b;
  my @list = reverse( map { $_->[0] } sort { $b->[1] <=> $a->[1] } map { [ $_, -M $_ ] }  readdir ( DIR ) );

  my($a, $b, $file, $s, $t);

    
##  sort { -M $A <=> -M $B } @fs );
  
  foreach( @list )
  {
    $a = "\L$_";
#    print ($a);


    if ( $_ eq ".." )
    {

	print ("\n  <tr>\n");
	print ("   <td>\n"); 
	print ("    <A HREF=\"../\"><IMG BORDER=0 SRC='/~motsak/imgs/dirup.gif' ALT='[DIRUP]'></A>\n"); 
	print ("   </td>\n"); 
	print ("   <td>\n"); 
	print ("    <A HREF=\"../\">Parent Directory</A>\n" );
	print ("   </td>\n"); 
	print ("   <td>\n"); 
	print ("    \n"); 
	print ("   </td>\n"); 
	print ("  </tr>\n");
    
    }
elsif( /^\s*\./ ) ## hidden files (system)
    {
    
    } 
elsif ( $_ ne "." && $_ ne ".." && $_ ne "index.html" )
    {
      $b = $a;
      $a = "$_[0]/$_";
      
      # get permissions...
      
      $t = ctime(stat($a)->mtime);

      if( -d $a ) ## Directory
      {

	print ("\n  <tr>\n");
	print ("   <td>\n"); 
	print ("    <A HREF=\"$a/\"><IMG BORDER=0 SRC='/~motsak/imgs/dir.gif' ALT='[DIR]'></A>\n"); 
	print ("   </td>\n"); 
	print ("   <td>\n"); 
	print ("    <A HREF=\"$a/\">$_</A>\n" );
	print ("   </td>\n"); 
	print ("   <td>\n"); 
	print ("    D: $t\n"); 
	print ("   </td>\n"); 
	print ("  </tr>\n");

#	myWalk("$_[0]/$_");	
      }else  ## File
      {    
      
        $file = "unknown.gif";
	
	     if ( $b =~ /\.((ps)|(ps\.gz)|(ps\.tar\.gz)|(ps\.bz2)|(ps\.tar\.bz2)|(ps\.tgz)|(ps\.zip))$/ )
	{
	    $file = "ps.gif";
	} elsif ( $b =~ /\.((tex)|(tex\.gz)|(tex\.tar\.gz)|(tex\.bz2)|(tex\.tar\.bz2)|(tex\.tgz)|(tex\.zip))$/ )
	{
	    $file = "tex.gif";
	} elsif ( $b =~ /\.((dvi)|(dvi\.gz)|(dvi\.tar\.gz)|(dvi\.bz2)|(dvi\.tar\.bz2)|(dvi\.tgz)|(dvi\.zip))$/ )
	{
	    $file = "dvi.gif";
	} elsif ( $b =~ /\.(tar)$/ )
	{
	    $file = "tar.gif";
	} elsif ( $b =~ /\.((tgz)|(tar\.gz)|(tar\.bz2)|(.zip))$/ )
	{
	    $file = "compressed.gif";
	} elsif ( $b =~ /\.((jpg)|(jpeg)|(bmp)|(tif)|(tiff)|(gif)|(pgm)|(png)|(avi)|(mov)|(pcx)|(scr)|(eps)|(pal))$/ )
	{
	    $file = "image.gif";
	} elsif ( $b =~ /\.((txt)|(doc)|(pdf)|(html)|(htm)|(shtml)|(sh)|(pl))$/ )
	{
	    $file = "text.gif";	
	};
	
       

	print ("\n  <tr>\n");
	print ("   <td>\n"); 
	print ("    <A HREF=\"$a\"><IMG BORDER=0 SRC='/~motsak/imgs/$file' ALT='[FILE]'></A>\n"); 
	print ("   </td>\n"); 
	print ("   <td>\n"); 
	print ("    <a href=\"$a\">$_</a>\n" );
	print ("   </td>\n"); 
	$s = stat($a)->size; 
	print ("   <td>\n"); 
	print ("    $t. size: $s\n");
	print ("   </td>\n"); 
	print ("  </tr>\n");

      };
    };
  };
  close(DIR);
};
