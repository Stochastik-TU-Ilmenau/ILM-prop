.PHONY: initialize
initialize:
	mkdir data/{interim,processed}
	cd src; Rscript -e dependencies.R

.PHONY: data
data: 
	cd src/data; Rscript -e interim.R; Rscript -e processed.R

.PHONY: submissions
submissions:
	cd src models; Rscript -e retrospective_nowcasts.R