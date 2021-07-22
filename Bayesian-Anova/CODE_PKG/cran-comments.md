## Test environments
* local R installation, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Resubmission 2020-07-20

Comments from reviewer Julia Haider:

`1.	Please add more details about the package functionality and implemented methods in your Description text. `
`Please do not start the description with "This package", package name, title or similar.`

`2.	If there are references describing the methods in your package, please`
`add these in the description field of your DESCRIPTION file in the form`
`authors (year) <doi:...>`
`authors (year) <arXiv:...>`
`authors (year, ISBN:...)`
`or if those are not available: <[https:...]https:...>`
`with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for`
`auto-linking.`
`(If you want to add a title as well please put it in quotes: "Title")`



`3. You have examples for unexported functions. aovbayes() in: aovbayes.Rd` 
`Please either omit these examples or export the functions.`

`4. Some code lines in examples are commented out in aovbayes.Rd.` 
`Please never do that. Ideally find toy examples that can be regularly`
`executed and checked. Lengthy examples (> 5 sec), can be wrapped in \donttest.`

`5. Please add \value to .Rd files regarding exported methods and explain the `
`functions results in the documentation. Please write about the structure of `
`the output (class) and also what the output means. (If a function does not `
`return a value, please document that too, e.g. \value{No return value, called`
` for side effects} or similar) Missing Rd-tags: aovbayes.Rd: \value `

In this presentation I have made the following changes. Comments made by reviewer Julia Haider were addressed:    
1. Added a more specific and detailed description of the package and corrected how to start writing.    
2. At the moment there are no references on the functionality or application of the package that can be cited in the description, we have an article that we hope to publish in a high impact journal such as rjournal when the package is published.    
3. The function has been exported and tested.    
4. The examples are no longer commented, now the examples can be run and checked.    
5. Added \value with the output description.    

