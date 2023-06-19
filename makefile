.PHONY: initialize
initialize:
	mkdir -p data/{interim,processed}
	cd src; Rscript dependencies.R

.PHONY: data
data: 
	cd src/data; Rscript interim.R; Rscript processed.R

.PHONY: submissions
submissions:
	cd src models; Rscript retrospective_nowcasts.R