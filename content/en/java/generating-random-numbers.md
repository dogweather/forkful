---
title:                "Generating random numbers"
html_title:           "Arduino recipe: Generating random numbers"
simple_title:         "Generating random numbers"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers is the process of creating a sequence that lacks any pattern. Programmers do this to ensure variability and unpredictability in their programs—vital for tasks like randomized tests, simulated experiments, or even for gaming logic.

## How to:

Java provides several ways to generate random numbers. Among them, using java.util.Random and java.util.concurrent.ThreadLocalRandom classes are common. Here's a small piece of code:

```Java
import java.util.Random;

public class MyClass {
  public static void main(String args[]) {
    Random rand = new Random();

    // Generate random integers in range 0 to 999
    int rand_int1 = rand.nextInt(1000);
    int rand_int2 = rand.nextInt(1000);

    // Print random integers
    System.out.println("Random Integers: "+rand_int1);
    System.out.println("Random Integers: "+rand_int2);
  }
}
```
When this code runs, it might output:

```
Random Integers: 636
Random Integers: 512
```

Notice the range provided in the nextInt() method. It defines the upper limit for the random number generation.

## Deep Dive

*Historical context*: The concept of generating random numbers dates back to ancient times, used primarily for games and divination. The advent of computers shifted their importance toward fields like simulation and cryptography.

*Alternatives*: Besides the Random and ThreadLocalRandom classes, you can also use java.lang.Math.random() method and java.util.stream API to generate random numbers in Java.

*Implementation details*: The standard Random class uses a linear congruential generator (LCG) to generate random numbers. However, for multi-threaded environments, ThreadLocalRandom is more efficient as it reduces contention and overhead typical of shared Random objects.

## See Also

- Oracle Java Documentation: Random (https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/util/Random.html)
- Oracle Java Documentation: ThreadLocalRandom (https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/util/concurrent/ThreadLocalRandom.html)
- java.lang.Math (https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#random--)
- java.util.stream (https://docs.oracle.com/javase/8/docs/api/java/util/stream/package-summary.html)