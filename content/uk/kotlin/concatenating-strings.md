---
title:                "Конкатенація рядків"
html_title:           "PHP: Конкатенація рядків"
simple_title:         "Конкатенація рядків"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що і навіщо?
Конкатенація рядків - це об'єднання двох чи більше рядків в один. Програмісти використовують її для створення комбінованих рядків з декількох джерел.

## Як це робиться:
Рядки можна об'єднувати в Kotlin за допомогою оператора "+". Приклад:

```Kotlin
val str1 = "Hello, "
val str2 = "World!"
val result = str1 + str2
println(result) //"Hello, World!"
```
Використовуючи інтерполяцію рядків в Kotlin, ми можемо об'єднати рядки інакше:
```Kotlin
val str1 = "Hello"
val str2 = ", World!"
val result = "$str1$str2"
println(result) // "Hello, World!"
```
## Поглиблений огляд:
Конкатенація рядків була важливою частиною мов програмування від їхнього початку. Вона дозволяє нам маніпулювати і динамічно створювати рядки, що є необхідним в абсолютної більшості програм.

Альтернативи до конкатенації рядків включають використання StringBuilder або StringBuffer в JVM-мовах, таких як Kotlin.

Щодо деталей виконання, важливо відзначити, що не всі методи конкатенації рядків рівні за продуктивністю. Опитування показує, що StringBuilder часто є найшвидшим способом, особливо для великих рядків або великої кількості операцій конкатенації.

## Більше матеріалів:
[Документація Kotlin про рядки](https://kotlinlang.org/docs/basic-types.html#string-literals)
[Tutorial: Рядки в Kotlin](https://www.programiz.com/kotlin-programming/string)
[StackOverflow: "Кращий спосіб з'єднати рядки в Kotlin"](https://stackoverflow.com/questions/44172162/best-way-to-concatenate-strings-in-kotlin)