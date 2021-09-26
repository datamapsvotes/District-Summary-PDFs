# District-Summary-PDFs
An R project to create a summary of polling booth results for Australian elections, with pdf summaries for each district.

## So, what is the point of this?
Initially, this project was intended to list margins for redivisions in the state of Victoria. Combined with a seperate mapping project, it since turned into a project to be able to map the political implications of these new boundaries as well.

Thus, this project allows for a nice summary of the redivisions, by whatever result that can be broken down by polling booth. It also allows for a simple polling booth breakdown of election results so people can get an idea of how geography influences the outcome in a given electorate.

## What does this R project contain?
This R project contains 2 R scripts and an R markdown document. It also contains data to analyse the 2019 Australian Federal Election, as well as the results of that analysis which can be found in 'joined.pdf'

`Functions.R` is simply listing the functions used in a way similar to a package.
`District Summary.Rmd` compiles the `rs_election` object with data, and compiles the graphics which appear on the PDF.
`Iterate.R` simply provides the code to provide multiple one page summaries, one for each electorate.

## Do I need anything else to run this code?
Yes, run the following code in the RStudio Terminal in order to install the `color` javascript package. This is used to generate the color pallettes for each party.
```
npm install -g browserify
npm install color
echo "global.color = require('color');" > in.js
browserify in.js -o out.js
```
