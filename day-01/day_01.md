# AOC Day 1

# Day 1: Trebuchet?!

Something is wrong with global snow production, and you’ve been selected
to take a look. The Elves have even given you a map; on it, they’ve used
stars to mark the top fifty locations that are likely to be having
problems.

You’ve been doing this long enough to know that to restore snow
operations, you need to check all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on
each day in the Advent calendar; the second puzzle is unlocked when you
complete the first. Each puzzle grants one star. Good luck!

You try to ask why they can’t just use a weather machine (“not powerful
enough”) and where they’re even sending you (“the sky”) and why your map
looks mostly blank (“you sure ask a lot of questions”) and hang on did
you just say the sky (“of course, where do you think snow comes from”)
when you realize that the Elves are already loading you into a trebuchet
(“please hold still, we need to strap you in”).

As they’re making the final adjustments, they discover that their
calibration document (your puzzle input) has been amended by a very
young Elf who was apparently just excited to show off her art skills.
Consequently, the Elves are having trouble reading the values on the
document.

The newly-improved calibration document consists of lines of text; each
line originally contained a specific calibration value that the Elves
now need to recover. On each line, the calibration value can be found by
combining the first digit and the last digit (in that order) to form a
single two-digit number.

For example:

1abc2 pqr3stu8vwx a1b2c3d4e5f treb7uchet In this example, the
calibration values of these four lines are 12, 38, 15, and 77. Adding
these together produces 142.

Consider your entire calibration document. What is the sum of all of the
calibration values?

## R Solution

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.4     ✔ readr     2.1.4
    ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ✔ purrr     1.0.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
# part 1
tibble(calibration_values = read_lines("input.txt")) %>%
  mutate(numbers = str_extract_all(calibration_values, "[:digit:]"),
         numbers = map_chr(numbers, ~str_c(.x, collapse = "")),
         numbers_first = str_extract(numbers, "[0-9]"),
         numbers_last = str_extract(numbers, regex("(\\d)(?!.*\\d)")),
         adder = pmap_chr(list(numbers_first, numbers_last),
                          ~str_c(..1, ..2, collapse = "")),
         adder = as.numeric(adder)) %>%
  pull(adder) %>%
  sum()
```

    [1] 55621

## Python Solution

``` python
import re
import numpy as np

calibration_values = open('input.txt').read()
calibration_values = calibration_values.split('\n')
size = len(calibration_values)

numbers_first = [''] * size
numbers_last = [''] * size
adders = [''] * size
total = 0
for value in np.arange(size):
  numbers_first[value] = re.search(r'\d', calibration_values[value]).group()
  numbers_last[value] = re.search(r'(\d)(?!.*\d)', calibration_values[value]).group()
  adders[value] = numbers_first[value] + numbers_last[value]
  adders[value] = int(adders[value])
  total += adders[value]

total
```

    55621

# Part 2

Your calculation isn’t quite right. It looks like some of the digits are
actually spelled out with letters: one, two, three, four, five, six,
seven, eight, and nine also count as valid “digits”.

Equipped with this new information, you now need to find the real first
and last digit on each line. For example:

two1nine eightwothree abcone2threexyz xtwone3four 4nineeightseven2
zoneight234 7pqrstsixteen In this example, the calibration values are
29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.

What is the sum of all of the calibration values?

## R Solution

``` r
search_expr <- "\\d|one|two|three|four|five|six|seven|eight|nine"

# function for returning the last match
detect_last <- function(string) {
  
  matchers <-
    c(as.character(1:9), 
      "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  
  match_idx <- 0
  for (i in 1:length(matchers)) {
    match_end <- str_locate_all(string, matchers[i])[[1]][,2]
    if (length(match_end) == 0) {
      match_end <- 0
    } else {
      match_end <- max(match_end)
    }
    if (match_end > match_idx) {
      last <- matchers[i]
      match_idx <- match_end
    }
    
  }
  
  return(last)
  
}

tibble(calibration_values = read_lines("input.txt")) %>%
  mutate(numbers = str_extract_all(calibration_values, "[:digit:]|one|two|three|four|five|six|seven|eight|nine"),
         numbers = map_chr(numbers, ~str_c(.x, collapse = "")),
         numbers_first = str_extract(numbers, "[0-9]|one|two|three|four|five|six|seven|eight|nine"),
         numbers_last = map_chr(calibration_values, detect_last),
         across(starts_with("numbers"), 
                ~case_match(.x,
                            "one" ~ "1",
                            "two" ~ "2",
                            "three" ~ "3",
                            "four" ~ "4",
                            "five" ~ "5",
                            "six" ~ "6",
                            "seven" ~ "7",
                            "eight" ~ "8",
                            "nine" ~ "9",
                            .default = .x)),
         adders = pmap_chr(list(numbers_first, numbers_last),
                           ~str_c(..1, ..2, collapse = "")),
         adders = as.numeric(adders)) %>% 
  pull(adders) %>%
  sum()
```

    [1] 53592

## Python Solution

``` python
def switch(string):
  switch_dict = {
    'one': 1,
    'two': 2,
    'three': 3,
    'four': 4,
    'five': 5,
    'six': 6,
    'seven': 7,
    'eight': 8,
    'nine': 9
  }
  
  return switch_dict.get(string, string)

def detect_match(string):
  
  matchers = ['1', '2', '3', '4', '5', '6', '7', '8', '9',
              'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine']
              
  match_idx = -1
  for i in np.arange(len(matchers)):
    match_end = [match.start() for match in re.finditer(matchers[i], string)]
    if len(match_end) == 0:
      match_end = -1
    else:
      match_end = match_end[len(match_end) - 1]
      
    if match_end > match_idx:
      last = matchers[i]
      match_idx = match_end
      
  return last

total = 0
for value in np.arange(size):
  numbers_first = re.search(r'\d|one|two|three|four|five|six|seven|eight|nine', calibration_values[value]).group()
  numbers_last = detect_match(calibration_values[value])
  numbers_first = str(switch(numbers_first))
  numbers_last = str(switch(numbers_last))
  adders = numbers_first + numbers_last
  adders = int(adders)
  total += adders
  
total
```

    53592
