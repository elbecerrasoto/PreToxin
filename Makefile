PTTG_codes.tsv: PTTG_codes.tsv.tar.gz
	tar -xvf $<

.PHONY style:
style:
	Rscript -e 'styler::style_dir(".", recursive=FALSE)'
