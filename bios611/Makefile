.PHONY: clean 

clean:
	rm -rf figure
	rm -rf report.html
	rm -rf .created-dirs

.created-dirs:
	mkdir -p figure
	touch .created-dirs

figure/FinalProject_Box.png: .created-dirs r/draw_box.R data/kc_house_data.csv
	Rscript	r/draw_box.R

figure/FinalProject_Map.png: .created-dirs r/draw_map.R data/map.png data/plot_price.png
	Rscript	r/draw_map.R

figure/FinalProject_Ridge.png: .created-dirs r/draw_ridge.R data/kc_house_data.csv
	Rscript	r/draw_ridge.R

figure/FinalProject_Scatter.png: .created-dirs r/draw_scatter.R data/kc_house_data.csv
	Rscript	r/draw_scatter.R

figure/FinalProject_PriceYear.png: .created-dirs r/draw_priceyear.R data/kc_house_data.csv
	Rscript	r/draw_priceyear.R

figure/FinalProject_PriceRange.png: .created-dirs r/draw_pricerange.R data/kc_house_data.csv
	Rscript	r/draw_pricerange.R
	
report.html: .created-dirs figure/FinalProject_Box.png figure/FinalProject_Map.png figure/FinalProject_Ridge.png figure/FinalProject_Scatter.png figure/FinalProject_PriceRange.png figure/FinalProject_PriceYear.png data/kc_house_data.csv data/map.png data/plot_price.png
	Rscript -e "rmarkdown::render(\"report.Rmd\", output_format=\"html_document\")"


