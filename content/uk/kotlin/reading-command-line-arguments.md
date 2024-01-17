---
title:                "Читання аргументів командного рядка."
html_title:           "Kotlin: Читання аргументів командного рядка."
simple_title:         "Читання аргументів командного рядка."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Що та чому?
Зчитування аргументів командного рядка є процесом, коли програміст використовує передані користувачем параметри для налаштування програми. Це дозволяє створювати більш гнучкі та налаштовані програми, що можуть працювати з різними наборами даних або поводитися по-різному в залежності від переданих параметрів.

Як це зробити:
```Kotlin
fun main(args: Array<String>) {
    // reading command line arguments
    val arg1 = args.getOrNull(0) // 1st argument
    val arg2 = args.getOrElse(1) { "default value" } // 2nd argument with default value
    val arg3 = args[2].toIntOrNull() ?: 0 // 3rd argument converted to integer or default value 0
    // sample output
    println("Arguments passed: $arg1, $arg2, $arg3")
}
```

Глибокий занурення:
Зчитування аргументів командного рядка стало популярним та важливим процесом з появою інтерактивних програм та командних оболонок. Існують альтернативні шляхи отримання параметрів, такі як зчитування з файлу конфігурації або інтерактивне введення користувачем. У Kotlin цей процес виконується з використанням вбудованих змінних, які доступні у функції main.

Дивись також:
Детальнішу інформацію про роботу з аргументами командного рядка можна знайти на офіційному сайті Kotlin: https://kotlinlang.org/docs/tutorials/kotlin-for-py/command-line.html