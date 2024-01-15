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

## Для чого

Злиття рядків є важливою операцією в програмуванні, оскільки дозволяє об'єднати окремі рядки в один, що є корисним, коли вам потрібно згенерувати велику кількість тексту або побудувати складніші рядки з даними.

## Як

```Kotlin
val firstName = "Іван"
val lastName = "Петренко"
val fullName = "$firstName $lastName"
println(fullName) //"Іван Петренко"
```

У цьому прикладі ми створюємо змінні для імені та прізвища, а потім зливаємо їх в один рядок за допомогою оператора долара "$" та знака "$" між ними. В результаті, отримуємо рядок "Іван Петренко", який виводимо на екран за допомогою функції `println()`.

```Kotlin
val age = 25
val message = "Мені " + age + " років"
println(message) //"Мені 25 років"
```

У цьому прикладі ми також додаємо змінну до рядка, але цього разу використовуємо оператор "+" та конкатенуємо рядки вручну. Результатом буде рядок "Мені 25 років".

## Глибше

Замість використання знаків "$" або "+", ви також можете скористатися функцією `format()`, щоб злити рядки за допомогою шаблону:

```Kotlin
val num = 123
val price = 99.99
val result = "Ціна товару: ".format(num, price)
println(result) //"Ціна товару: 123 99.99"
```

Також варто відзначити, що злиття рядків може бути не тільки змінними, але і виразами та функціями:

```Kotlin
val num = 10
val result = "Квадрат числа ${num * num}"
println(result) //"Квадрат числа 100"
```

## Дивись також

- Документація: https://kotlinlang.org/docs/reference/basic-types.html#strings
- Medium: https://medium.com/@aromajoin/kotlin-string-templates-7a2a96b9439f
- Відео урок: https://www.youtube.com/watch?v=HFXmKFYcadE