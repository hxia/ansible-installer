

open(IN, "t1") || die "$! \n";

while(<IN>){
 chomp;
 print "- name: install R {{ r_version }} package: $_\n";
 print "  command: chdir=/usr/local/R-{{ r_version }}/bin ./Rscript -e \"install.packages('$_', repos='http://cran.us.r-project.org')\" \n";
 print "\n";
}

close(IN);

