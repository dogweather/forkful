---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers is an essential task in programming that involves creating a sequence of numbers that cannot be predicted or repeated. Programmers use random numbers for a variety of purposes, including generating unique IDs, simulating unpredictable behavior, and selecting items randomly from a list.

## How to:

To generate random numbers in Arduino, we can use the ```random()``` function. This function takes two parameters: the minimum and maximum values of the range of numbers we want to generate. For example, if we want to generate a random number between 1 and 10, we would use the following code:

```
Arduino int rand_num = random(1,10);
```

We can also use the ```randomSeed()``` function to set a seed value for the random number generator. This seed value is used to generate the sequence of numbers, so by changing the seed value, we can get a different sequence of random numbers. This is useful when we want to have consistent results in our program.

To generate a random number within a certain range but with a specific increment, we can use the ```random(x, y, z)``` function. The third parameter, z, specifies the increment value. For example, if we want to generate a random number between 1 and 10, with increments of 2, we would use the following code:

```
Arduino int rand_num = random(1,10,2);
```

## Deep Dive:

There are two main approaches to generating random numbers: pseudo-random number generators and true random number generators. Pseudo-random number generators use a mathematical algorithm to generate a sequence of numbers that appears random but is actually deterministic. True random number generators, on the other hand, use physical phenomena (such as atmospheric noise) to generate truly random numbers.

In Arduino, the ```random()``` function uses a pseudo-random number generator, specifically the 'linear congruential generator' (LCG) algorithm. This algorithm generates numbers based on a seed value and a set of mathematical calculations. While this may not produce truly random numbers, it is sufficient for most applications.

Another alternative for generating random numbers in Arduino is by using external hardware, such as an analog noise source or a dedicated random number generator chip. However, these methods may require additional components and may not be necessary for most projects.

It's essential to note that the ```random()``` function is not truly random, and it is possible for the same sequence of numbers to be generated if the same seed value is used. For most applications, this may not be an issue, but for more secure use cases, it's essential to use alternative methods for generating random numbers.

## See Also:

- Official Documentation for random(): https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- Overview of Pseudo-random Number Generators: https://www.geeksforgeeks.org/pseudo-random-number-generator-prng/
- True Random Number Generators: https://www.howtogeek.com/183051/htg-explains-how-computers-generate-random-numbers/