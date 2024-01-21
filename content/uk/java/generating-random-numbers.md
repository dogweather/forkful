---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:49:21.344876-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Генерація випадкових чисел — це про створення непередбачуваних значень. Програмісти це роблять для тестування, ігор, симуляцій та шифрування.

## How to:
Java дає нам клас `Random` для генерації випадкових чисел. Ось як це працює:

```java
import java.util.Random;

public class RandomExample {
    public static void main(String[] args) {
        Random random = new Random();

        int randomInt = random.nextInt(100); // цифра від 0 до 99
        double randomDouble = random.nextDouble(); // число від 0.0 до 1.0
        boolean randomBoolean = random.nextBoolean(); // істина чи брехня
        
        System.out.println("Random Int: " + randomInt);
        System.out.println("Random Double: " + randomDouble);
        System.out.println("Random Boolean: " + randomBoolean);
    }
}
```

Запуск видасть вас три випадкових значення, наприклад:

```
Random Int: 42
Random Double: 0.8403
Random Boolean: true
```

## Deep Dive:
У минулому використовували `Math.random()` для простих завдань, але він менш гнучкий ніж `Random`. Також є `SecureRandom` для криптографічних потреб. Хоч `Random` і простий в користуванні, він не для критичних до безпеки сценаріїв.

Рандомізатори працюють на псевдовипадкових числах, що створюються алгоритмами і можуть бути відтворені з того ж початкового значення (seed).

## See Also:
- [Oracle Java Documentation on Random Class](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Oracle Java Documentation on SecureRandom Class](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
- [Stack Overflow Discussion on Random Number Generation](https://stackoverflow.com/questions/363681/how-do-i-generate-random-integers-within-a-specific-range-in-java)