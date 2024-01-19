---
title:                "Видобування підрядків"
html_title:           "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

# Видобування підрядків у Kotlin: Що, чому і як?
## Що і чому?
Видобування підрядків полягає в отриманні частини рядка. Ми робимо це, коли нам потрібно працювати тільки з деякими частинами рядка, а не з цілим рядком.

## Як це виконати:
У Kotlin для видобування підрядків ми використовуємо метод `substring()`. Ось як це працює:

```Kotlin
fun main() {
    val str = "Привіт, світ Kotlin!"
    val subStr = str.substring(7, 11)
    println(subStr)  // Виведе: світ
}
```
## Поглиблений огляд:
Метод `substring()` був уведений у Kotlin від початку, тому він став основним способом видобування підрядків в цій мові. 

Альтернативами можуть бути методи такі як `slice()`, але вони зазвичай використовуються, коли потрібно видобути декілька підрядків одночасно. 

Усередині, Kotlin використовує Java-реалізацію підрядків, що дозволяє ефективно працювати з пам'яттю.

## Дивись також:
1. Документація по `substring()`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/substring.html
2. Огляд рядків у Kotlin: https://kotlinlang.org/docs/basic-types.html#strings
3. Способи роботи з рядками у Kotlin: https://www.programiz.com/kotlin-programming/string