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

## Why 

Generating random numbers is a simple yet powerful tool for programmers. It allows for the creation of dynamic and unpredictable elements in code, making it perfect for tasks such as simulations, gaming, and secure encryption.

## How To
To generate random numbers in Java, we can use the built-in `Random` class. First, we need to import the class at the top of our code: 
```Java
import java.util.Random;
```
Next, we can create an instance of the `Random` class and use its methods to generate different types of random numbers. For example, to generate a random integer between 0 and 100 (inclusive), we can use the `nextInt()` method:
```Java
Random random = new Random();
int randomNumber = random.nextInt(101);
System.out.println(randomNumber);
```
The output of this code will be a random number between 0 and 100. We can also generate random floating-point numbers using the `nextDouble()` method:
```Java
double randomDouble = random.nextDouble();
System.out.println(randomDouble);
```
This will print a random number between 0.0 (inclusive) and 1.0 (exclusive). You can also use other methods from the `Random` class to generate different types of random numbers, such as `nextBoolean()`, `nextLong()`, and `nextGaussian()`. 

## Deep Dive 
Behind the scenes, the `Random` class uses a mathematical algorithm called a pseudorandom number generator to generate random numbers. The seed value, which is used as the starting point for the algorithm, can be specified when creating an instance of the `Random` class. This seed value determines the sequence of random numbers that will be generated.

It is important to note that although these numbers may appear random to us, they are actually generated through a deterministic process. This means that if we use the same seed value, we will always get the same sequence of random numbers. 

## See Also 
- [Java Random Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Generating Random Numbers in Java](https://www.geeksforgeeks.org/generating-random-numbers-in-java/)
- [Understanding Pseudorandom Number Generators](https://medium.com/@penguinmishra/random-numbers-does-it-exist-in-nature-6aad9ded0008)