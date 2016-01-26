

open(IN, "t1") || die "$! \n";

while(<IN>){
 chomp;
 print "- name: install R package $_\n";
 print "  command: chdir=/usr/local/R-3.2.1/bin ./Rscript -e \"install.packages('$_', repos='http://cran.us.r-project.org')\" \n";
 print "\n";
}

close(IN);

