---
title:                "Generating random numbers"
date:                  2024-01-20T17:49:28.435509-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generating random numbers"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers means creating unpredictable values. Programmers use randomness for games, simulations, testing, and security.

## How to:
Java provides a few ways to generate random numbers. We'll look at `Random`, `Math.random()`, and `ThreadLocalRandom`.

### Using `Random`:
```java
import java.util.Random;

public class RandomExample {
    public static void main(String[] args) {
        Random random = new Random();

        // Generates a random integer between 0 (inclusive) and the specified value (exclusive).
        int randomInt = random.nextInt(100);
        System.out.println("Random Integer: " + randomInt);

        // Generates a random double between 0.0 and 1.0.
        double randomDouble = random.nextDouble();
        System.out.println("Random Double: " + randomDouble);
    }
}
```

Sample Output:
```
Random Integer: 45
Random Double: 0.7918273
```

### Using `Math.random()`:
```java
public class MathRandomExample {
    public static void main(String[] args) {
        
        // Generates a random double between 0.0 (inclusive) and 1.0 (exclusive).
        double randomDouble = Math.random();
        System.out.println("Random Double: " + randomDouble);
    }
}
```

Sample Output:
```
Random Double: 0.4137723
```

### Using `ThreadLocalRandom`:
```java
import java.util.concurrent.ThreadLocalRandom;

public class ThreadLocalRandomExample {
    public static void main(String[] args) {
        
        // Generates a random integer between 0 (inclusive) and 100 (exclusive).
        int randomInt = ThreadLocalRandom.current().nextInt(0, 100);
        System.out.println("Random Integer: " + randomInt);

        // Generates a random double between 0.0 (inclusive) and 1.0 (exclusive).
        double randomDouble = ThreadLocalRandom.current().nextDouble(0.0, 1.0);
        System.out.println("Random Double: " + randomDouble);
    }
}
```

Sample Output:
```
Random Integer: 82
Random Double: 0.6654892
```

## Deep Dive:
Before Java 1.2, the only option was `Math.random()`. Later, `Random` was introduced, providing more flexibility. `Random` is thread-safe but can become a bottleneck in concurrent applications. `ThreadLocalRandom` solves this in Java 7 by giving each thread its own `Random` instance, reducing contention and improving performance in multi-threaded environments.

Alternatives to these methods include using `SecureRandom` for cryptographic needs. It's part of `java.security` and guarantees a higher degree of randomness.

Implementation relies on algorithms like Linear Congruential Generator (LCG) in `Random`. It's decent for casual use but not for high-level security or scientific precision.

## See Also:
- Oracle's official documentation on `Random`: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html
- More about `Math.random()` : https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#random--
- Java 8 `ThreadLocalRandom` guide: https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadLocalRandom.html
- Understanding `SecureRandom`: https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html