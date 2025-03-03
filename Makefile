PTTG_codes.tsv: PTTG_codes.tsv.tar.gz
	tar -xvf $<

EXCLUDE = exclude_dirs = c("packrat", "renv", "results")
.PHONY style:
style:
	Rscript -e 'styler::style_dir(".", $(EXCLUDE))'
