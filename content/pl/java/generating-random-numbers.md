---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:49:42.072052-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Generowanie liczb losowych to proces tworzenia sekwencji liczb, które nie mają wzorca. Programiści korzystają z tych sekwencji m.in. w grach, symulacjach, testowaniu oprogramowania oraz w systemach zabezpieczeń.

## Jak to zrobić:
W Javie, najprostszym sposobem na generowanie liczb losowych jest użycie klasy `Random` z pakietu `java.util`. Oto przykład:

```java
import java.util.Random;

public class RandomNumbers {
    public static void main(String[] args) {
        Random random = new Random();

        // Generuje losową liczbę całkowitą.
        int randomInt = random.nextInt();
        System.out.println("Losowa liczba całkowita: " + randomInt);

        // Generuje losową liczbę całkowitą z zakresu (0, 100).
        int randomInt100 = random.nextInt(100);
        System.out.println("Losowa liczba całkowita (0-99): " + randomInt100);

        // Generuje losową liczbę zmiennoprzecinkową.
        double randomDouble = random.nextDouble();
        System.out.println("Losowa liczba zmiennoprzecinkowa (0.0-1.0): " + randomDouble);
    }
}
```

Sample output (wynik przykładowy):

```
Losowa liczba całkowita: 1955835968
Losowa liczba całkowita (0-99): 37
Losowa liczba zmiennoprzecinkowa (0.0-1.0): 0.5348782376
```

## Głębokie zanurzenie
Historia generowania liczb losowych sięga czasów przed komputerami - używano różnych metod fizycznych, jak rzuty kostką. W informatyce, algorytmy generowania liczb pseudolosowych (PRNGs) odgrywają kluczową rolę. Są to algorytmy deterministyczne, ale ich wyniki wyglądają na losowe. Alternatywą dla `Random` jest klasa `SecureRandom` dla wymagań bezpieczeństwa, która jest mocniejsza kryptoanalitycznie. `ThreadLocalRandom` to dobry wybór dla aplikacji wielowątkowych. Od Java 7 można też używać `SplittableRandom`, która jest szybsza niż `Random` i pozwala na generowanie liczb w parach dla użytku w równoległym przetwarzaniu danych.

## Zobacz również
- [Oracle's Java Documentation on Random Class](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Random.html)
- [SecureRandom](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
- [ThreadLocalRandom](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadLocalRandom.html)
- [Math.random() javadoc](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#random--)
- [The use of randomness in cryptography](https://en.wikipedia.org/wiki/Cryptography)