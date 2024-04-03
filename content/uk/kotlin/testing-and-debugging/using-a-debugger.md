---
date: 2024-01-26 03:50:26.828139-07:00
description: "\u041E\u0441\u044C \u043C\u0430\u043B\u0435\u043D\u044C\u043A\u0430\
  \ \u0434\u0435\u0433\u0443\u0441\u0442\u0430\u0446\u0456\u044F \u0432\u0456\u0434\
  \u043B\u0430\u0433\u043E\u0434\u0436\u0435\u043D\u043D\u044F \u043D\u0430 Kotlin\
  \ \u0437 IntelliJ IDEA - Sherlock Holmes \u0441\u0435\u0440\u0435\u0434 IDE: ```kotlin\
  \ fun main() { val mysteryNumber = 42 var guess = 0\u2026"
lastmod: '2024-03-13T22:44:49.226807-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0441\u044C \u043C\u0430\u043B\u0435\u043D\u044C\u043A\u0430 \u0434\
  \u0435\u0433\u0443\u0441\u0442\u0430\u0446\u0456\u044F \u0432\u0456\u0434\u043B\u0430\
  \u0433\u043E\u0434\u0436\u0435\u043D\u043D\u044F \u043D\u0430 Kotlin \u0437 IntelliJ\
  \ IDEA - Sherlock Holmes \u0441\u0435\u0440\u0435\u0434 IDE:\n\n```kotlin\nfun main()\
  \ {\n    val mysteryNumber = 42\n    var guess = 0\n\n    while (guess ."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0434\
  \u0435\u0431\u0430\u0433\u0435\u0440\u0430"
weight: 35
---

## Як це зробити:
Ось маленька дегустація відлагодження на Kotlin з IntelliJ IDEA - Sherlock Holmes серед IDE:

```kotlin
fun main() {
    val mysteryNumber = 42
    var guess = 0

    while (guess != mysteryNumber) {
        println("Вгадай число: ")
        guess = readLine()?.toIntOrNull() ?: continue // Ігноруємо неправильні вводи

        // Встановіть точку переривання тут, щоб спостерігати 'guess' в дії
        if (guess < mysteryNumber) {
            println("Занадто мало!")
        } else if (guess > mysteryNumber) {
            println("Занадто багато!")
        }
    }

    println("Ви вгадали! Таємниче число було $mysteryNumber")
}
```

Вивід дебаггера:
```
Вгадай число: 
10
Занадто мало!
Вгадай число: 
50
Занадто багато!
Вгадай число: 
42
Ви вгадали! Таємниче число було 42
```

## Поглиблене вивчення
Дебаггери існують у грі з 50-х років. Тоді вони були досить примітивними, і відлагодження могло бути більше про апаратне забезпечення, ніж про програмне. На сьогодні, дебаггер, як у IntelliJ IDEA, дозволяє нам встановлювати точки переривання, проходити через код рядок за рядком, і інспектувати стан змінних, коли нам зручно.

Хоча дебаггер IntelliJ є дуже зручним для Kotlin, він не єдиний у морі. Є ряд альтернатив, як Logcat для розробки Android, або командні інструменти, як jdb для мінімалістів. Магія під капотом тут - це переважно про JVM Tool Interface (JVMTI), яка дозволяє дебаггерам взаємодіяти з Java віртуальною машиною, тримаючи розробників Kotlin у курсі справ.

## Дивіться також
- Документація дебаггера IntelliJ IDEA: [https://jetbrains.com/idea/](https://www.jetbrains.com/idea/features/debugger.html)
