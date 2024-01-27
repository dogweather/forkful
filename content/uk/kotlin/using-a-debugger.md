---
title:                "Використання дебагера"
date:                  2024-01-26T03:50:26.828139-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання дебагера"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/using-a-debugger.md"
---

{{< edit_this_page >}}

## Що і чому?
Погруження у використання дебаггера - це все про крок за кроком проходження через ваш код, спостереження за тим, як все працює, і виловлювання тих надокучливих помилок на гарячому. Програмісти використовують дебаггери, тому що це детективні інструменти, які допомагають нам з'ясувати, де щось пішло не так, не вириваючи при цьому волосся.

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
- Документація Kotlin щодо відлагодження: [https://kotlinlang.org/docs/debugging.html](https://kotlinlang.org/docs/debugging.html)
- Корені відлагодження у історії: [http://history-computer.com/Internet/Maturing/Debugging.html](http://history-computer.com/Internet/Maturing/Debugging.html)