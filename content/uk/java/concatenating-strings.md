---
title:                "Конкатенація рядків"
html_title:           "PHP: Конкатенація рядків"
simple_title:         "Конкатенація рядків"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що & Навіщо?
Конкатенація рядків - це процес з'єднання двох або більше рядків. Програмісти роблять це, щоб виводити або обробляти більш складні рядкові структури.

## Як зробити:
```java
// Створюємо два рядки
String string1 = "Привіт, ";
String string2 = "світе!";

// Конкатенуємо рядки
String greeting = string1 + string2;

// Виводимо результат
System.out.println(greeting);
```

В результаті ви отримаєте рядок: "Привіт, світе!".

## Глибше занурення
Конкатенація виникла ще з перших днів програмування і досі є основним способом об'єднання рядків. Однак, в Java є інші методи, такі як StringBuilder і StringBuffer. Ці класи подають собою змінні рядки, які можуть бути ефективнішими при об'єднаннi великих об'ємів даних.

Спосіб, яким Java реалізує конкатенацію, - через оператор "+". Але варто знати, що кожна конкатенація використовує нову пам'ять і створює новий об'єкт. Тому для великих завдань рекомендується використовувати StringBuilder або StringBuffer.

## Дивіться також
1. [StringBuilder tutorial](https://docs.oracle.com/javase/tutorial/java/data/buffers.html)
2. [StringBuffer tutorial](https://docs.oracle.com/javase/9/docs/api/java/lang/StringBuffer.html)
3. [Метод String.concat()](https://www.tutorialspoint.com/java/java_string_concat.htm)