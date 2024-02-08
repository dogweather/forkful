---
title:                "Визначення довжини рядка"
aliases:
- uk/kotlin/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:54.941138-07:00
model:                 gpt-4-1106-preview
simple_title:         "Визначення довжини рядка"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

## Що та Навіщо?
Визначення довжини рядка дозволяє вам знати, скільки символів міститься в ньому. Програмісти роблять це для валідації вводу, обрізання тексту, для подальших маніпуляцій з даними.

## How to:

## Як зробити:
```kotlin
fun main() {
    val exampleString = "Вітаємо!"
    val lengthOfExampleString = exampleString.length
    println("Довжина рядка: $lengthOfExampleString")
}
```
Вивід:
```
Довжина рядка: 9
```

## Deep Dive

## Поглиблений Підхід
У Kotlin, як і в Java, рядки представлені класом `String`, що зберігає символи у масиві. Метод `.length` повертає кількість символів. Це корисно, але пам'ятайте, що він не завжди відображає кількість візуально сприйнятих символів через використання UTF-16 кодування, де деякі символи можуть займати два 'кодові пункти'. Альтернативою є використання `.codePointCount()`, особливо коли працюєте з Unicode. Історично, мірою оптимізації рядків були серії оновлень до структур даних, які їх представляли, починаючи з простих об'єктів масивів, і далі до складних форм зі збереженням символів та довжини.

## See Also

## Додатково
- [Kotlin String documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Stack Overflow: Counting Unicode characters](https://stackoverflow.com/questions/1735110/count-number-of-unicode-characters-in-a-string-with-kotlin)
