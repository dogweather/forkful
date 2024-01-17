---
title:                "Generating random numbers"
html_title:           "Java recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers means producing a sequence of numbers that appear to be chosen at random. This is useful for a variety of purposes in programming, such as generating passwords, simulating random events, and creating unique identifiers. 

Programmers use random numbers to add an element of unpredictability to their programs, making them more realistic and secure. It allows for more dynamic and varied outcomes, making programs more versatile and engaging for users.

## How to:
To generate random numbers in Java, we can use the built-in ```Random``` class. Here's an example of generating a random integer between 0 and 10:
```Java
import java.util.Random;

public class RandomExample {
   public static void main(String[] args) {
      // create a Random object
      Random random = new Random();
      // generate a random integer between 0 and 10
      int randomNumber = random.nextInt(11);
      // print the generated number
      System.out.println("Random number: " + randomNumber);
   }
}
```
Sample output:
```
Random number: 7
```

We can also generate random numbers within a specific range by using arithmetic operations with the generated number. For example, to generate a random number between 50 and 100, we can use the following code:
```Java
// generate a random number between 50 and 100
int randomNumber = random.nextInt(51) + 50;
```

## Deep Dive:
The concept of random numbers can be traced back to ancient civilizations, where methods like rolling dice or drawing straws were used for decision-making. In the world of computing, the first random number generator was developed in 1969 by John von Neumann. Since then, many different algorithms and techniques have been created for generating random numbers.

In Java, the `Random` class uses a variant of the linear congruential generator (LCG) algorithm to generate random numbers. This algorithm uses a mathematical formula to produce seemingly random numbers based on a starting value called the seed.

However, it is important to note that these numbers are not truly random, as they are generated using a deterministic algorithm. They are referred to as pseudo-random numbers. To achieve truly random numbers, specialized hardware or external sources of randomness are needed.

## See Also:
- [Java's Random class Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Comparison of Pseudo-Random Number Generators](https://en.wikipedia.org/wiki/Comparison_of_pseudo-random_number_generators)
- [Random Number Generation in Java 8](https://dzone.com/articles/random-number-generation-java-8)