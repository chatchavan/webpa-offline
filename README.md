# Peer assessment rating template generator and adjustment factor calculation

WebPA is a tool for grading students' groupwork in a fair manner. Students rate and give feedback to each of their group member—including themselves. Their ratings are used to adjust the group-score. Students who were rated higher by their group members will receive a positive adjustment slightly higher than the group-score. The opposite applies for students who were rated lower.

WebPA was originally developed by Loughborough University as an open-source web tool [[WebPA project website]](http://webpaproject.lboro.ac.uk) [[Github]](https://github.com/WebPA/WebPA) [[WebPA scoring algorithm](http://webpaproject.lboro.ac.uk/academic-guidance/a-worked-example-of-the-scoring-algorithm/). 

We developed an offline version to mitigate cybersecurity and GDPR concerns.


## Requirements
* R packages
   * `tidyverse`
   * `openxlsx`

## Usage
### 1. Generating rating templates for students
1. Create an excel file with columns:
  - *Person*: full name of the student (example of entry: "John Doe");
  - *Team*: team number 1..N (examples of entry: 1, 15, 10)
2. Save the created file to  `input/students_groups.xlsx`.
3. Open and run `R/generate_form.R`
4. The generated files will be saved to `output/template_for_students`.

### 2. Calculate the adjustment factor based on the ratings

1. You will need the `input/students_groups.xlsx` from above

2. Put the ratings received from students in Excel in `input/submitted_ratings`. Each group should be stored in one folder. For example
   * `input/submitted_ratings/group_01/01.xlsx`
   * `input/submitted_ratings/group_01/02.xlsx`
   * `input/submitted_ratings/group_01/05.xlsx`
   * `input/submitted_ratings/group_02/01.xlsx`

3. Run the `R/calculate_adjustments.R`

4. The grade adjustment factor will be saved as `output/webpa_score.csv`.


## Generating example files
To generate example forms and calculation based on [the example on the WebPA page](http://webpaproject.lboro.ac.uk/academic-guidance/a-worked-example-of-the-scoring-algorithm/), use the following files as input:

* `examples/students_groups.xlsx`
* `examples/template.xlsx`

Follow the step described above to generate templates for students (`R/generate_form.R`). Then, run `R/generate_examples.R` to generate filled-rating files in `input/submitted_ratings/group1`.

## Contributors
* Written and maintained by [Chat Wacharamanotham](http://chatchavan.github.io/) and 
* Initial version of the code: [Natalia Obukhova](https://www.obukhova.org)
* Contributed to code reviewing: [Alexander Eiselmayer](https://eiselmayer.com)