---
title:                "Java: Об'єднання рядків"
simple_title:         "Об'єднання рядків"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому

Переписування коду зазвичай є неприємним процесом для багатьох програмістів, проте деякі завдання вимагають цього. З'єднання рядків може стати важливим елементом у процесі написання програм, особливо, якщо потрібно створити текст з динамічними даними.

## Як

```Java
String name = "Олексій";
String age = "21";
String message = "Привіт, мене звати " + name + " і мені " + age + " років!";
System.out.println(message);
```

Виведе на екран: "Привіт, мене звати Олексій і мені 21 років!"

## Глибше вдивимося

В Java існує декілька способів працювати з рядками і з'єднувати їх. Одним із найпростіших та найбільш часто використовуваних є оператор "+".

Ще одним підходом є використання методу `concat()`, який приймає рядок, який потрібно додати до базового рядка.

Наприклад:

```Java
String hello = "Привіт, ";
String name = "Олена";
String message = hello.concat(name);
System.out.println(message);
```

Виведе на екран: "Привіт, Олена"

Існує також метод `String.format()`, який дозволяє створювати рядки за допомогою шаблонів. Наприклад, ми можемо замість конкатенації використати такий підхід:

```Java
String name = "Василь";
int age = 25;
String message = String.format("Мене звати %s і мені %d років!", name, age);
System.out.println(message);
```

Виведе на екран: "Мене звати Василь і мені 25 років!"

Варто також згадати, що рядки можна з'єднувати за допомогою класу `StringBuilder`, який дозволяє ефективніше маніпулювати рядками.

## Дивіться також

- [Документація Java String](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [10 способів з'єднання рядків в Java](https://www.baeldung.com/java-string-concatenation)