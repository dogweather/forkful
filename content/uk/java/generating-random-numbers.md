---
title:                "Java: Створення випадкових чисел"
simple_title:         "Створення випадкових чисел"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

Почему: Якщо ви хочете створювати різні предмети або є потреба випадкових значень у вашій програмі, генерування випадкових чисел може бути дуже корисним інструментом для вас.

Як: Для генерування випадкового числа у Java, вам потрібно використовувати клас Random. Ось приклад коду і виводу за допомогою Random у Java:

```Java
import java.util.Random;

public class RandomNumberGenerator{
    public static void main(String[] args){

        //Створення екземпляру класу Random
        Random random = new Random();

        //Генерування випадкового цілого числа
        int randomInt = random.nextInt();

        //Виведення результатів на екран
        System.out.println("Випадкове ціле число: " + randomInt);
    }
}
```

Вивід: Випадкове ціле число: -1430458179

Глибокий занурення: Існує багато методів в класі Random, які дозволяють генерувати випадкові числа за певними додатковими умовами, наприклад в зазначеному діапазоні або в певній послідовності. Також, в Java 8 була додана можливість генерувати випадкові числа за допомогою методів у класі ThreadLocalRandom.

Додатково, ви також можете використовувати клас Math для генерування випадкових чисел. Цей клас містить статичний метод random(), який повертає випадкове число з плаваючою точкою в діапазоні [0, 1). Ось приклад використання методу random():

```Java
import java.lang.Math;

public class RandomNumberGenerator{
    public static void main(String[] args){

        //Генерування випадкового числа з плаваючою точкою
        double randomDouble = Math.random();

        //Виведення результатів на екран
        System.out.println("Випадкове число з плаваючою точкою: " + randomDouble);
    }
}
```

Вивід: Випадкове число з плаваючою точкою: 0.7504949547823672

Див. також: Щоб дізнатися більше про клас Random та інші способи генерування випадкових чисел у Java, рекомендуємо переглянути наступні ресурси:

- https://www.geeksforgeeks.org/generating-random-numbers-in-java/
- https://www.javatpoint.com/java-math-random-method
- https://docs.oracle.com/javase/8/docs/api/java/util/Random.html