---
title:                "З'єднання рядків"
html_title:           "Kotlin: З'єднання рядків"
simple_title:         "З'єднання рядків"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що & Чому?
З'єднання строк у програмуванні - це процес об'єднання двох або більше рядків у один. Це часто використовується для побудови нового рядку з існуючих даних або для форматування виведення. Програмісти використовують з'єднання строк для полегшення роботи зі строковими даними та покращення ефективності свого коду.

## Як:
```Kotlin
val hello = "Привіт"
val name = "Олександр"
val greeting = hello + " " + name + ", як справи?"
println(greeting)
```
Виведе: Привіт Олександр, як справи?

```Kotlin
val firstWord = "Вітання"
val secondWord = "з"
val thirdWord = " Kotlin!"
val phrase = firstWord + secondWord + thirdWord
println(phrase)
```
Виведе: Вітання з Kotlin!

## Глибше:
З'єднання строк вже стало необхідною частиною роботи з даними у багатьох мовах програмування. Цей процес дозволяє швидше та ефективніше маніпулювати даними, а також полегшує розробку складних програм. У деяких мовах програмування, таких як Java та C++, існують спеціальні функції для з'єднання строк, але у Kotlin це можна зробити простим додаванням рядків за допомогою оператора "+".

## Дивись також:
Детальніше про з'єднання строк у Kotlin можна прочитати у [документації](https://kotlinlang.org/docs/reference/basic-types.html#string-concatenation). Також можна порівняти цей процес з альтернативними методами з'єднання строк, наприклад, за допомогою функцій ```StringBuilder``` та ```StringBuffer``` в Java.