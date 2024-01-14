---
title:    "Kotlin: Записуючи рядок"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Чому

Капіталізація рядка є важливою частиною програмування, оскільки вона дозволяє налагоджувати й легко розуміти дані, які виводяться на екран.

## Як

Для капіталізації рядка використовується функція `capitalize()`, яка приймає на вхід рядок та повертає копію цього рядка з першою літерою великими буквами.

```Kotlin
val str = "ukraine"
println(str.capitalize())

// Output: Ukraine
```

Також можна використовувати функцію `toUpperCase()`, яка перетворює всі літери рядка великими.

```Kotlin
val str = "kyiv"
println(str.toUpperCase())

// Output: KYIV
```

## Глибокий занурення

Коли ми використовуємо методи капіталізації, ми створюємо новий рядок, а не змінюємо оригінальний. Також, важливо пам'ятати, що методи капіталізації не впливають на роздільники та не перетворюють їх на великі літери.

## Дивитися також

- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Java String capitalize vs toUpperCase](https://stackoverflow.com/questions/3055639/java-string-capitalize-vs-touppercase)
- [Kotlin Standard Functions](https://kotlinlang.org/docs/reference/scope-functions.html)