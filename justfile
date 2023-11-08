# run the pipeline
run:
    Rscript model.R
    git add --all
    git commit -m "update with model run"
    git push origin main