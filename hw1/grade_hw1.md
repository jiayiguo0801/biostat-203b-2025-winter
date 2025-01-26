*jiayi guo*

### Overall Grade: 78/130

### Quality of report: 8/10

-   Is the homework submitted (git tag time) before deadline? Take 10 pts off per day for late submission.  

-   Is the final report in a human readable format html and pdf? 

-   Is the report prepared as a dynamic document (Quarto) for better reproducibility?

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how results are produced by just reading the report? Take points off if the solutions are too succinct to grasp, or there are too many typos/grammar.

**The html file is inconsistent with qmd file.** (-2 pts)

### Completeness, correctness and efficiency of solution: 56/80

- Q1 (10/10)

	Is the GitHub.com repository name set up correctly? If using name other than `biostat-203b-2025-winter`, take 5 points off.

- Q2 (18/20)

	If CITI training is not completed successfully, take 15 points off. 
	
	If PhysioNet crecential is not complete, take 5 pts off.
	
	**Please display the link or screenshot in the homework.** (-2 pts)

- Q3 (12/20)

	Q3.1, if the gz files are ever decompressed or copied in the solutions, take 5 points off.
	
	For Q3.5-7, should skip the header when finding the unique values of each variable. Take 5 points of if not done so.
	
	**No correct output for Q3.5-8, no interpretation.** (-8 pts)

- Q4 (6/10)

	It's fine to just count the lines containing each name. If a student figures out a way to count the words (one line may contain the same name multiple times), give bonus points.
	
	**Q4.3: This command will display the last 5 lines of the first 20 lines of the file pg42671.txt.** (-4 pts)

- Q5 (5/10)

    **Not give interpretations.** (-5 pts)

- Q6 (5/10)

    **Do not use absolute path.** (-5 pts)
	    
### Usage of Git: 5/10

-   Are branches (`main` and `develop`) correctly set up? Is the hw submission put into the `main` branch?

-   Are there enough commits (>=5) in develop branch? Are commit messages clear? The commits should span out, not clustered the day before deadline.
    
    **Only 3 commits.** (-5 pts)
          
-   Is the hw1 submission tagged? 

-   Are the folders (`hw1`, `hw2`, ...) created correctly? 
  
-   Do not put auxiliary files into version control. If files such as `.Rhistory`, `.RData`, `.Rproj.user`, `.DS_Store`, etc., are in Git, take 5 points off.

-   If those gz data files or `pg42671` are in Git, take 5 points off.

### Reproducibility: 5/10

-   Are the materials (files and instructions) submitted to the `main` branch sufficient for reproducing all the results? Just click the `Render` button will produce the final `html`? 

-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

    **Do not use absolute path in the homework.** (-5 pts)

### R code style: 4/20

For bash commands, only enforce the 80-character rule. Take 2 pts off for each violation. 

-   [Rule 2.6](https://style.tidyverse.org/syntax.html#long-function-calls) The maximum line length is 80 characters. Long URLs and strings are exceptions.  
    **Line 89, 90, 91, 97, 98, 99, 100, 107**

-   [Rule 2.5.1](https://style.tidyverse.org/syntax.html#indenting) When indenting your code, use two spaces.  

-   [Rule 2.2.4](https://style.tidyverse.org/syntax.html#infix-operators) Place spaces around all infix operators (=, +, -, &lt;-, etc.).  

-   [Rule 2.2.1.](https://style.tidyverse.org/syntax.html#commas) Do not place a space before a comma, but always place one after a comma.  

-   [Rule 2.2.2](https://style.tidyverse.org/syntax.html#parentheses) Do not place spaces around code in parentheses or square brackets. Place a space before left parenthesis, except in a function call.
