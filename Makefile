.PHONY help:
help:
	less Makefile

codes.tsv.tar.gz: codes.tsv
	tar -czvf $@ $<

codes.tsv: codes.tsv.tar.gz
	tar -xvf $<

.PHONY style:
style:
	Rscript -e 'styler::style_dir(".", recursive=FALSE)'
