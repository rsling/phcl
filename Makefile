LX = xelatex
MI = makeindex
BX = biber

FILEN = glmm
TEXFLAGS =

once:
	-$(LX) $(TEXFLAGS) $(FILEN) | grep 'Warning\|Error'
	@echo
	@echo " == WORD COUNT == "
	@echo
	@texcount -merge -total -utf8 -incbib -v0 glmm.tex

quick:
	-$(LX) $(TEXFLAGS) $(PREFLAGS) $(FILEN)
	printf "\033c"
	@echo
	@echo " === LAST RUN === "
	@echo
	-$(LX) $(TEXFLAGS) $(FILEN) | grep 'Warning'
	@echo
	@echo " == WORD COUNT == "
	@echo
	@texcount -merge -total -utf8 -incbib -v0 glmm.tex

clean:
	\rm *.adx *.and *.aux *.bbl *.blg *.idx *.ilg *.ldx *.lnd *.log *.out *.rdx *.run.xml *.sdx *.snd *.toc *.wdx *.xdv *.bcf *.pdf *.xwm .DS_Store RPHCL/.DS_Store RPHCL/.RData RPHCL/.Rhistory RPHCL/Rplots.pdf graphics/.DS_Store sections/.DS_Store
	\rm -rf .Rproj.user

all:
	-$(LX) $(TEXFLAGS) $(PREFLAGS) $(FILEN)
	-$(BX) $(FILEN)
	-$(LX) $(TEXFLAGS) $(PREFLAGS) $(FILEN)
	printf "\033c"
	@echo
	@echo " === LAST RUN === "
	@echo
	-$(LX) $(TEXFLAGS) $(FILEN) | grep 'Warning\|Error'
	@echo
	@echo " == WORD COUNT == "
	@echo
	@texcount -merge -total -utf8 -incbib -v0 glmm.tex

view:
	open $(FILEN).pdf & 

edit:
	mvim -c ':set spell spelllang=en' -c ':nnoremap <F15> ]s' -c ':nnoremap <F14> [s' $(FILEN).tex
