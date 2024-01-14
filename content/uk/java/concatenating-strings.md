---
title:                "Java: Зливання рядків."
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому

Робота з об'єднанням рядків може бути корисною у випадках, коли необхідно з'єднати два чи більше рядки в один, або додати до певного рядка додаткову інформацію. Наприклад, це може бути корисно при створенні повідомлень для виведення на екран або при форматуванні даних для подальшої обробки.

## Як

```Java
// Створення рядків
String name = "Олександра";
String greeting = "Привіт, ";

// Об'єднання рядків за допомогою оператора +
String message = greeting + name;
System.out.println(message); // Виведе "Привіт, Олександра"

// Об'єднання рядків за допомогою методу concat()
String phrase = "Я люблю " 
String hobby = "програмування";
String result = phrase.concat(hobby);
System.out.println(result); // Виведе "Я люблю програмування"
```

## Глибокий дослідження

Об'єднання рядків виконується за допомогою оператора "+" або методу concat(). При цьому, є кілька нюансів, які варто враховувати при роботі з рядками:

- Об'єднання рядків займає більше часу та пам'яті, ніж робота зі змінними типу StringBuilder або StringBuffer.
- Рядки можна об'єднати також за допомогою методу join(), який знаходиться у класі StringJoiner.
- У разі об'єднання багатьох рядків, використання методу StringBuilder або StringBuffer є більш оптимальним, оскільки вони працюють зі змінними типу StringBuffer, що дозволяє уникнути надмірного створення об'єктів типу String.

## Дивись також

- [Офіційна документація Java з роботою з рядками](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Стаття про роботу з рядками на блозі JavaCafe](https://javacafe.org.ua/articles/jdk/strings-0)
- [Відеоурок з роботи з рядками на YouTube](https://www.youtube.com/watch?v=S_Ck7743Jes)