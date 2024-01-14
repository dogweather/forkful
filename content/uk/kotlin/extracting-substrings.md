---
title:                "Kotlin: Отримання підстрок"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Чому

Екстрагування підрядка є важливою властивістю мови Kotlin. Ця функціональність дозволяє вам отримувати лише потрібну частину рядка, що є корисним у багатьох сценаріях програмування.

## Як це зробити

Для того, щоб екстрагувати підрядок, використовуйте метод `substring()` з об'єктом рядка:

```Kotlin
val str = "Привіт, світ!"
val substring = str.substring(0, 6)
println(substring) // виведе "Привіт"
```

Цей код вибере перші шість символів рядка `str` і збереже їх у змінній `substring`. Для вибору підрядка використовується індексування символів: перший символ має індекс 0, останній - n-1, де n - загальна кількість символів.

## Вивчення поглибленніше

За допомогою методу `substring()` можна вибирати не тільки підрядки, що розташовані вище початкового рядка, але і ті, що розташовані після нього. Для цього можна використовувати негативні значення індексів. Наприклад:

```Kotlin
val str = "Hello, world!"
val substring = str.substring(7)
println(substring) // виведе "world!"
```

У цьому випадку, метод `substring()` поверне усі символи рядка `str` після сьомого символу (включаючи його).

## Дивіться також

* [Документація Kotlin про метод `substring()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
* [Стаття про метод `substring()` на сайті TutorialsPoint](https://www.tutorialspoint.com/kotlin/kotlin_string_substrings.htm)