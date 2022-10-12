'''
Code to render the RMarkdown file and create the README.md file
'''

rmarkdown::render("ST 558 Project 2.Rmd", output_file = "README.md", \
                  params=list(state='NC', ordering='desc', rows = 100, \
                              year = 'latest', ownership = 'private', \
                              locale = 'town'))