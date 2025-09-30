.PHONY: help install preview render clean test lint

help:
	@echo "BZ-Data Makefile"
	@echo "================"
	@echo "install    - Install R dependencies with renv"
	@echo "preview    - Preview Quarto site"
	@echo "render     - Render Quarto site"
	@echo "shiny      - Run Shiny app"
	@echo "clean      - Clean generated files"
	@echo "test       - Run R tests"
	@echo "lint       - Lint R code"

install:
	Rscript -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv'); renv::restore()"

preview:
	quarto preview

render:
	quarto render

shiny:
	Rscript -e "shiny::runApp('apps/shiny')"

clean:
	rm -rf _site/ _freeze/ .quarto/
	find . -name "*.html" -type f -delete
	find . -name "*_cache" -type d -exec rm -rf {} +
	find . -name "*_files" -type d -exec rm -rf {} +

test:
	Rscript -e "testthat::test_dir('tests/testthat')"

lint:
	Rscript -e "lintr::lint_dir('R')"

