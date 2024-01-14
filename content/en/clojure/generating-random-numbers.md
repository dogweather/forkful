---
title:                "Clojure recipe: Generating random numbers"
programming_language: "Clojure"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Why Generating Random Numbers in Clojure is Important

If you are a programmer, you may have heard or used the term "random numbers" before. Random numbers are essential for a variety of tasks, such as building games, simulations, and statistical analysis. In Clojure, generating random numbers is a crucial part of creating dynamic and unpredictable programs.

# How To Generate Random Numbers in Clojure

To generate a random number in Clojure, you can use the ```rand``` function. This function takes in a single argument, which represents the upper bound of the random number range. For example, to generate a random number between 1 and 10, you would use the following code:

```Clojure
(rand 10)
```

The output of this code might be something like 4.72882 or 9.00689. If you need to generate an integer instead of a decimal, you can wrap the ```rand``` function inside the ```int``` function, like this:

```Clojure
(int (rand 10))
```

This will return a whole number between 1 and 10, such as 3 or 8.

You can also use the ```rand-int``` function to generate random integers within a specified range. This function takes in two arguments, the lower and upper bound of the range. For example, to generate a random number between 50 and 100, you would use the following code:

```Clojure
(rand-int 50 100)
```

The output of this code could be 72 or 94. You can also use the ```rand-nth``` function to generate a random element from a given collection, such as a list or vector.

# Deep Dive into Generating Random Numbers in Clojure

Under the hood, the ```rand``` function uses Java's ```java.lang.Math.random()``` method to generate random numbers. This method uses a pseudorandom algorithm and depends on the current system time to generate its values.

Clojure also provides the ```random``` function, which allows you to specify the seed for your random number generator. This can be useful if you need to generate the same set of random numbers multiple times, or if you want to control the randomness in your program.

Additionally, Clojure provides several other functions for generating random numbers, such as ```rand-long```, ```rand-float```, and ```rand-double``` for generating different types of numbers. You can also use these functions to create sequences of random numbers using the ```repeatedly``` function.

# See Also

- Official Clojure Documentation: https://clojure.org/reference/data_structures#_random_numbers
- Clojure API Reference: https://clojuredocs.org/clojure.core/rand
- Tutorialspoint article on Generating Random Numbers in Clojure: https://www.tutorialspoint.com/clojure/clojure_random_numbers.htm