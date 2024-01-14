---
title:    "Java recipe: Generating random numbers"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Why Generate Random Numbers in Java?

Generating random numbers in Java can be extremely useful in various applications, such as simulations, games, and statistical analysis. It allows for the creation of unpredictable values, which can be used to test and validate algorithms, as well as add elements of randomness to a program.

## How To Generate Random Numbers in Java

Generating random numbers in Java can be easily done using the `java.util.Random` class. Below is a simple example that generates and prints 5 random integers between 1 and 10:

```Java
import java.util.Random;

public class RandomNumbers {
    public static void main(String[] args) {
        Random rand = new Random();

        for(int i = 0; i < 5; i++) {
            int randomNum = rand.nextInt(10) + 1;
            System.out.println(randomNum);
        }
    }
}
```
Output:
```
7
5
10
2
8
```

The `nextInt()` method takes in an integer as the upper limit and returns a random integer between 0 (inclusive) and the given number (exclusive). Therefore, adding 1 to the result ensures that the random numbers generated are within the desired range.

Other commonly used methods from the `java.util.Random` class include `nextDouble()` for generating random decimal numbers and `nextBoolean()` for generating random boolean values.

## Deep Dive into Generating Random Numbers in Java

The `java.util.Random` class uses a mathematical formula to generate random numbers based on a seed value. The seed value can be set manually or it is based on the current time by default. This means that the same seed will produce the same sequence of random numbers every time it is run.

To produce truly random and unpredictable numbers, the `java.util.SecureRandom` class should be used instead. It uses a more secure algorithm and additional sources of randomness to generate the numbers.

Additionally, the `java.util.Random` class can also be used to generate random strings by using the `nextInt()` method to retrieve random characters from a given string.

## See Also

Here are some useful resources for further reading on generating random numbers in Java:

- [Java Random Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [SecureRandom in Java](https://www.baeldung.com/java-securerandom)
- [Generating Random Strings in Java](https://www.geeksforgeeks.org/generate-random-string-of-given-size-in-java/)