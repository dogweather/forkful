---
date: 2024-01-20 17:47:54.941138-07:00
description: ''
lastmod: '2024-03-13T22:44:49.204358-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0412\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u0434\u043E\u0432\
  \u0436\u0438\u043D\u0438 \u0440\u044F\u0434\u043A\u0430"
weight: 7
---

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
