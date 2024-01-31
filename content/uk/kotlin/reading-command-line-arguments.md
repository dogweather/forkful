---
title:                "Читання аргументів командного рядка"
date:                  2024-01-20T17:57:00.015516-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання аргументів командного рядка"

category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?
Читання аргументів командного рядка – це процес збору даних, які користувач передає вашій програмі при її запуску. Програмісти використовують це для налаштування поведінки програми без зміни коду.

## Як це зробити:
```kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        println("Привіт, ${args[0]}!")
    } else {
        println("Привіт, невідомий!")
    }
}

// Використання: > kotlinc hello.kt -include-runtime -d hello.jar
//               > java -jar hello.jar Василь
// Вивід: Привіт, Василь!
```

## Поглиблений огляд:
Читання аргументів командного рядка – це стара концепція, відома з часів перших днів програмування. В Kotlin це відбувається через масив `args`, який передається в метод `main()`. Хоча Kotlin призначений для роботи з JVM, він забезпечує синтаксичний цукор, такий як `vararg` для кращого досвіду при роботі з аргументами командного рядка. Альтернативи включают використання бібліотек, таких як `kotlinx-cli`, які надають більш продвинуті можливості для парсингу командного рядка.

## Див. також:
- Документація Kotlin про точку входу у програму: [kotlinlang.org](https://kotlinlang.org/docs/command-line.html)
- `kotlinx-cli`: [GitHub](https://github.com/Kotlin/kotlinx-cli)
