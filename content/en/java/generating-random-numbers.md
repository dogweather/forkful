---
title:                "Generating random numbers"
date:                  2024-01-27T19:45:12.044834-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generating random numbers"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

Generating random numbers in Java is a process of creating numbers that lack any sort of pattern, ensuring unpredictability. Programmers frequently utilize this capability for a variety of purposes, including testing and simulation, security, and gaming applications, as it enables the creation of dynamic and non-repeatable scenarios.

## How to:

Java offers multiple ways to generate random numbers, primarily through the `java.util.Random` and `java.util.concurrent.ThreadLocalRandom` classes, as well as the `Math.random()` method for simpler needs.

### Using `java.util.Random`:

This class provides methods to generate random boolean, integer, and floating-point numbers.

```Java
import java.util.Random;

public class RandomExample {
    public static void main(String[] args) {
        Random random = new Random();

        int randInt = random.nextInt(); // Generates any random integer
        double randDouble = random.nextDouble(); // Generates a random double between 0.0 and 1.0
        System.out.println("Random Integer: " + randInt);
        System.out.println("Random Double: " + randDouble);
    }
}
```

Sample output:
```
Random Integer: -2038438924
Random Double: 0.730967787376657
```

### Using `Math.random()`:

This method is handy for quick and straightforward tasks. It returns a double value between 0.0 (inclusive) and 1.0 (exclusive).

```Java
public class SimpleRandom {
    public static void main(String[] args) {
        double simpleRand = Math.random();
        System.out.println("Simple Random Double: " + simpleRand);
    }
}
```

Sample output:
```
Simple Random Double: 0.44023009870487915
```

### Using `java.util.concurrent.ThreadLocalRandom`:

For use in concurrent applications, this class minimizes contention among threads, making it a better choice than `java.util.Random` in multi-threaded environments.

```Java
import java.util.concurrent.ThreadLocalRandom;

public class ThreadLocalRandomExample {
    public static void main(String[] args) {
        int randInt = ThreadLocalRandom.current().nextInt(1, 51); // Generates a random integer between 1 and 50
        double randDouble = ThreadLocalRandom.current().nextDouble(1, 11); // Generates a random double between 1.0 and 10.0
        System.out.println("ThreadLocal Random Integer: " + randInt);
        System.out.println("ThreadLocal Random Double: " + randDouble);
    }
}
```

Sample output:
```
ThreadLocal Random Integer: 29
ThreadLocal Random Double: 5.123456789012345
```

## Deep Dive

Historically, achieving true randomness in computing has been a challenge, with early methods often relying on mathematical algorithms that could eventually repeat patterns. The `java.util.Random` class, introduced in JDK 1.0, relies on a linear congruential generator (LCG), which, while sufficient for basic randomness needs, is not considered strong enough for security-critical applications due to its predictable pattern after a certain point.

For better security, Java offers `java.security.SecureRandom`, a subclass of `java.util.Random` that provides a cryptographically strong random number generator (RNG). Unlike `Random`, `SecureRandom` accesses the native operating system's random number generation features, or uses a configurable algorithm to produce non-predictable and secure random numbers, which is vital for encryption, token generation, etc.

The choice between `Random`, `ThreadLocalRandom`, `Math.random()`, and `SecureRandom` depends on the application's specific requirements concerning performance, concurrent usage, and the level of security needed.

## See also

### Official Java Documentation
- [Java `java.util.Random` Class](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Java `java.security.SecureRandom` Class](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)

### Tutorials and Guides
#### Basic Random Number Generation
- **GeeksforGeeks**: [Random Number Generation in Java](https://www.geeksforgeeks.org/generating-random-numbers-in-java/)
- **JournalDev**: [Java Random Number Generator](https://www.journaldev.com/122/java-random-number-generator-example)
  
#### Secure Random Number Generation
- **Baeldung**: [Guide to java.security.SecureRandom](https://www.baeldung.com/java-secure-random)
- **HowToDoInJava**: [SecureRandom – Generating Secure Random Numbers](https://howtodoinjava.com/java8/secure-random-number-generation/)

#### Random Numbers with Streams
- **Baeldung**: [Generate Random Numbers Using Java 8 Streams](https://www.baeldung.com/java-8-random-numbers)
- **Stack Abuse**: [Generating Random Numbers in Java with Streams](https://stackabuse.com/generating-random-numbers-in-java-with-java-util-random/)
