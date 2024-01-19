---
title:                "Генерація випадкових чисел"
html_title:           "Java: Генерація випадкових чисел"
simple_title:         "Генерація випадкових чисел"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що та чому?

Генерація випадкових чисел - це процес створення чисел, які неможливо передбачити краще, ніж випадковим щурятівєм. Програмісти це роблять для шифрування даних, випробувань, ігор та багато іншого.

## Як створити:

```Java
import java.util.Random;

public class Main {
    public static void main(String[] args) {
        Random rand = new Random();
        int randomNum = rand.nextInt(50);
        System.out.println("Випадкове число: " + randomNum);
    }
}
```
Семпл виводу:
```Java
Випадкове число: 35
```

## Поглиблений розділ:

У 1946 році Джон вон Нейман вперше ввів спосіб генерації псевдовипадкових чисел. Але ці числа страждають від нестабільності: повторення цієї послідовності після деякого часу. Щоб уникнути цього, використовуйте криптографічно-безпечні генератори випадкових чисел.

В Java є декілька способів генерування випадкових чисел, наприклад, `Math.random()`, `Random()` або `ThreadLocalRandom()`. Вони працюють на основі ідеї лінійного конгруентного генератора.

## Дивись також:

1. Oracle Docs on `java.util.Random`: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html
2. Baeldung guide on generating random numbers in Java: https://www.baeldung.com/java-generate-random-long-float-integer-double
3. GeeksforGeeks tutorial on `java.util.Random`: https://www.geeksforgeeks.org/random-nextint-method-in-java-with-examples/