# College_Students_R
remediation analysis for College Students dropouts

This project consisted of an analysis of College Students dropouts from a sample of 1000 students. 

The dataset consisted of several .csv sources, College Entry test results, Academic information for each Subject, Class, test and project throught the duration of the career, as well as
prior academic background, financial information regarding socio-economics and tuition payment records.

1. Clean data, integrate data into a single dataframe (after performing some simple feature engineering to each source)
2. Upon the cleansing and integration of the data, I performed a cluster analysis, in order to identify groups of bad-performing students. Meaning, those that are more prone to drop-out.
Then proceeded to label the students in said group.  
3. Train a ML neural network to identify students falling into said "Dropout candidates" group
4. Develop an algorithm to propose remediation measures tailored to each student. This was a genetic algorithm that performed the analysis of each remediation measure, optimizing for
efficiency in moving students away from the Dropout candidates group and maximizing the resources available (money).
