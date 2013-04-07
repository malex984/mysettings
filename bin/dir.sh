#!/bin/sh

if [ ! -e index.html ]; then
        # start html!
	if [ ! -e .head ]; then
	   echo "<html>" >> .head
	   echo " <body>" >> .head
	   echo "  <form>" >> .head
	   echo "  <fieldset>" >> .head
	   echo "  <legend>Directory: `$HOME/bin/pwd.sh`</legend>" >> .head
	   chmod go-rwx .head
	fi
        cat .head > index.html
  
        # front mater: [header]?
        if [ -e .header ]; then
           cat .header >> index.html
	fi
 
        # main matter
        if [ -e .content ]; then
	   cat .content >> index.html
    	else
	   # simple file listing!
	   if [ ! -e .dir ]; then
	      $HOME/bin/dir.pl > .dir
	      chmod go-rwx .dir
	   fi
	
	   if [ -e .dir ]; then
	      cat .dir >> index.html
	   fi
        fi
	
	# back matter? [footer]?
        if [ -e .footer ]; then
           cat .footer >> index.html
	fi
	
	# finish html!
	if [ ! -e .foot ]; then
	   echo "  </fieldset>" > .foot
	   echo "  </form>" >> .foot
	   echo "  <HR><font size=\"2\"><ADDRESS>Generated `date +\"%A, %d %B %Y %H:%M:%S\"` at `hostname -s` by Alex::dir.sh and Co :)</ADDRESS>" >> .foot
	   echo " </body>" >> .foot
	   echo "</html>" >> .foot
	   chmod go-rwx .foot
	fi	
        cat .foot >> index.html
fi

if [ -e index.html ]; then
    chmod go-rwx index.html
    chmod o+r index.html    
fi  
