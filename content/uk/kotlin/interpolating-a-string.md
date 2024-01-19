---
title:                "Інтерполяція рядка"
html_title:           "Java: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Інтерполяція рядків у Kotlin - це спосіб вставки значень змінних прямо в середину рядка. Ми це робимо, щоби зробити код більш читабельним та зменшити кількість помилок.

## Як це робиться:

Тут невеличкий приклад кода, що показує, як використовувати інтерполяцію рядків в Kotlin.

```Kotlin
val name = "Andrew"
val age = 25
println("My name is $name and I am $age years old")
```

Виходь з цього прикладу:

```
My name is Andrew and I am 25 years old
```

## Поглиблений аналіз:

Варто знати, що інтерполяція рядків була добавлена в Kotlin після спостереження за схожими механізмами в інших мовах, наприклад в Ruby або Python. Альтернативою цьому може бути старий стиль конкатенації рядків, але він вважається менш читабельним.

При реалізації інтерполяції рядків, Kotlin використовує заміну рядка на значення змінної вилучаючи '$'. 

## Див. також:

Більше деталі про інтерполяцію рядків у Kotlin ви зможете знайти тут:

[Kotlin Language Documentation](https://kotlinlang.org/docs/reference/basic-types.html#string-templates)

[Guide To Kotlin String Interpolation](https://www.baeldung.com/kotlin-string-interpolation)

[Understanding String Interpolation in Kotlin](https://jivimberg.io/blog/2018/05/04/string-interpolation-in-kotlin)