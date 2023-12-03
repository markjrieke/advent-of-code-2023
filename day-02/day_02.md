# AOC Day 2

# Day 2: Cube Conundrum

You’re launched high into the atmosphere! The apex of your trajectory
just barely reaches the surface of a large island floating in the sky.
You gently land in a fluffy pile of leaves. It’s quite cold, but you
don’t see much snow. An Elf runs over to greet you.

The Elf explains that you’ve arrived at Snow Island and apologizes for
the lack of snow. He’ll be happy to explain the situation, but it’s a
bit of a walk, so you have some time. They don’t get many visitors up
here; would you like to play a game in the meantime?

As you walk, the Elf shows you a small bag and some cubes which are
either red, green, or blue. Each time you play this game, he will hide a
secret number of cubes of each color in the bag, and your goal is to
figure out information about the number of cubes.

To get information, once a bag has been loaded with cubes, the Elf will
reach into the bag, grab a handful of random cubes, show them to you,
and then put them back in the bag. He’ll do this a few times per game.

You play several games and record the information from each game (your
puzzle input). Each game is listed with its ID number (like the 11 in
Game 11: …) followed by a semicolon-separated list of subsets of cubes
that were revealed from the bag (like 3 red, 5 green, 4 blue).

For example, the record of a few games might look like this:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green Game 2: 1 blue, 2
green; 3 green, 4 blue, 1 red; 1 green, 1 blue Game 3: 8 green, 6 blue,
20 red; 5 blue, 4 red, 13 green; 5 green, 1 red Game 4: 1 green, 3 red,
6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red Game 5: 6 red, 1 blue,
3 green; 2 blue, 1 red, 2 green In game 1, three sets of cubes are
revealed from the bag (and then put back again). The first set is 3 blue
cubes and 4 red cubes; the second set is 1 red cube, 2 green cubes, and
6 blue cubes; the third set is only 2 green cubes.

The Elf would first like to know which games would have been possible if
the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

In the example above, games 1, 2, and 5 would have been possible if the
bag had been loaded with that configuration. However, game 3 would have
been impossible because at one point the Elf showed you 20 red cubes at
once; similarly, game 4 would also have been impossible because the Elf
showed you 15 blue cubes at once. If you add up the IDs of the games
that would have been possible, you get 8.

Determine which games would have been possible if the bag had been
loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes. What
is the sum of the IDs of those games?

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
red_max <- 12
green_max <- 13
blue_max <- 14

color_max <- function(text, color) {
  
  text %>%
    str_extract_all(paste0("(\\d+)\\s*", color)) %>%
    pluck(1) %>%
    str_c(collapse = "; ") %>%
    str_extract_all("(\\d+)") %>%
    pluck(1) %>%
    as.integer() %>%
    max()
  
}

tibble(input = read_lines("input.txt")) %>% 
  mutate(game = str_extract(input, regex("^.*?(?=:)")),
         game = as.integer(str_remove_all(game, "Game ")),
         red = map_int(input, ~color_max(.x, "red")),
         green = map_int(input, ~color_max(.x, "green")),
         blue = map_int(input, ~color_max(.x, "blue"))) %>%
  filter(red <= red_max,
         green <= green_max,
         blue <= blue_max) %>%
  pull(game) %>%
  sum()
```

    [1] 2268

## Python Solution

``` python
import numpy as np
import re

red_max = 12
green_max = 13
blue_max = 14

def game_num(text):
  game = int(re.search(r'\d+', text).group())
  return game

def color_max(text, color):
  draws = re.findall(rf'(\d+)\s*{color}', text)
  draws = [int(i) for i in draws]
  return max(draws)
  
input_txt = open('input.txt').read()
input_txt = input_txt.split('\n')

total = 0
for game in np.arange(len(input_txt)):
  gid = game_num(input_txt[game])
  red = color_max(input_txt[game], 'red')
  green = color_max(input_txt[game], 'green')
  blue = color_max(input_txt[game], 'blue')
  
  if red <= red_max and green <= green_max and blue <= blue_max:
    total += gid
    
total
```

    2268

# Part 2

The Elf says they’ve stopped producing snow because they aren’t getting
any water! He isn’t sure why the water stopped; however, he can show you
how to get to the water source to check it out for yourself. It’s just
up ahead!

As you continue your walk, the Elf poses a second question: in each game
you played, what is the fewest number of cubes of each color that could
have been in the bag to make the game possible?

Again consider the example games from earlier:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green Game 2: 1 blue, 2
green; 3 green, 4 blue, 1 red; 1 green, 1 blue Game 3: 8 green, 6 blue,
20 red; 5 blue, 4 red, 13 green; 5 green, 1 red Game 4: 1 green, 3 red,
6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red Game 5: 6 red, 1 blue,
3 green; 2 blue, 1 red, 2 green In game 1, the game could have been
played with as few as 4 red, 2 green, and 6 blue cubes. If any color had
even one fewer cube, the game would have been impossible. Game 2 could
have been played with a minimum of 1 red, 3 green, and 4 blue cubes.
Game 3 must have been played with at least 20 red, 13 green, and 6 blue
cubes. Game 4 required at least 14 red, 3 green, and 15 blue cubes. Game
5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the bag. The
power of a set of cubes is equal to the numbers of red, green, and blue
cubes multiplied together. The power of the minimum set of cubes in game
1 is 48. In games 2-5 it was 12, 1560, 630, and 36, respectively. Adding
up these five powers produces the sum 2286.

For each game, find the minimum set of cubes that must have been
present. What is the sum of the power of these sets?

## R Solution

``` r
tibble(input = read_lines("input.txt")) %>%
  mutate(red = map_int(input, ~color_max(.x, "red")),
         green = map_int(input, ~color_max(.x, "green")),
         blue = map_int(input, ~color_max(.x, "blue")),
         power = red * green * blue) %>%
  pull(power) %>%
  sum()
```

    [1] 63542

## Python Solution

``` python
total = 0
for game in np.arange(len(input_txt)):
  red = color_max(input_txt[game], 'red')
  green = color_max(input_txt[game], 'green')
  blue = color_max(input_txt[game], 'blue')
  
  power = red * green * blue
  total += power
    
total
```

    63542
