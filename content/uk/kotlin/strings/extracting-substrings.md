---
title:                "Виділення підрядків"
aliases:
- /uk/kotlin/extracting-substrings/
date:                  2024-01-20T17:46:04.031025-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виділення підрядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Що таке отримання підрядків та навіщо це програмістам?

Отримання підрядків - це метод виділення частини тексту з рядка. Програмісти це роблять для аналізу, обробки даних або, наприклад, для валідації вводу в програмі.

## How to:
Ось як це робиться в Kotlin:

```Kotlin
fun main() {
    val text = "Привіт, як твої справи?"

    // Витягти підрядок від 7 до 19 символа
    val substring = text.substring(7, 20) // "як твої справи"
    println(substring)

    // Витягти підрядок з використанням діапазону
    val rangeSubstring = text.slice(7..19) // "як твої справи"
    println(rangeSubstring)
}
```

Вихідний результат обох методів однаковий:
```
як твої справи
як твої справи
```

## Deep Dive:
В повсякденному кодингу, отримання підрядків - фундаментальний інструмент. В Kotlin, метод `substring` з’явився з Java, оскільки Kotlin побудований так, щоб бути сумісним з Java бібліотеками. Альтернативно, можна використовувати `slice`, який приймає діапазони і забезпечує більш гнучкий спосіб виділення тексту.

Під капотом, ці методи оптимізовані для ефективної роботи з рядками, але варто пам’ятати про можливі `StringIndexOutOfBoundsException`, які можуть виникати при спробі доступу до неіснуючих індексів.

## See Also:
Для подальшого читання та навчання:
- [Kotlin documentation on substring](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- [Oracle Java String documentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html) - адже розуміння Java допоможе з Kotlin
- [Kotlin Range Expressions](https://kotlinlang.org/docs/ranges.html)
