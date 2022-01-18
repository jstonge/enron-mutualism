all: clean get-data preprocess find-communities find-topics mutualism	

clean:
	rm output/* figs/*

get-data:
	cd raw-data && sh download-enron.sh
	
preprocess:
	cd src/ && python 1_1-preprocessing.py && Rscript 1_2-preprocessing.R
	
find-communities:
	cd src/ && python 2-find_communities.py

find-topics:
	cd src/ && Rscript 3-find_topics.R
	
mutualism:
	cd src/ && Rscript plot_fig2.R && Rscript plot_fig3.R