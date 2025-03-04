.PHONY help:
help:
	less Makefile

data.tar.gz: data
	tar -czvf $@ $<

data: data.tar.gz
	tar -xvf $<

.PHONY style:
style:
	Rscript -e 'styler::style_dir(".", recursive=FALSE)'

.PHONY clean:
	rm data.tar.gz
