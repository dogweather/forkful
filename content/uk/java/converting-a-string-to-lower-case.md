---
title:                "Перетворення рядка на нижній регістр."
html_title:           "Java: Перетворення рядка на нижній регістр."
simple_title:         "Перетворення рядка на нижній регістр."
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

Написання і читання інформації в коді є однією з основних частин програмування. При обробці текстових даних, може знадобитися змінити регістр символів. У цій статті ми розглянемо, як перетворити рядок на нижній регістр за допомогою Java.

## Як

```java
String str = "HELLO WORLD";
String lowercase = str.toLowerCase();
System.out.println(lowercase);
```
Вивід: `hello world`
Строка `"HELLO WORLD"` була перетворена на `"hello world"` за допомогою методу `toLowerCase()`. Він повертає копію рядка зі зниженим регістром всіх символів.

## Глибоке занурення

У Java існує ще один метод для перетворення рядка на нижній регістр - `toLowerCase(Locale locale)`. Він дозволяє вказати необхідну локаль, для якої буде проводитися перетворення. Наприклад, існують мови, у яких є специфічні правила для перетворення регістру, наприклад, турецька. В цьому випадку, ми можемо використовувати `Locale` для того, щоб отримати коректний результат.

Наприклад:

```java
String str = "İstanbul";
String lowercase = str.toLowerCase(new Locale("tr"));
System.out.println(lowercase);
```
Вивід: `istanbul`

Без використання `Locale` результат був би `"i̇stanbul"`. Пам'ятайте, що використовуючи `toLowerCase(Locale locale)`, потрібно оголосити об'єкт `Locale`. Наприклад: `Locale.setDefault(new Locale("tr"));`.

## Дивись також

- [Java Documentation: String toLowerCase()](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)
- [Java Documentation: Locale](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html)