---
title:    "Kotlin: Перетворення рядка в нижній регістр"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Чому

Іноді при програмуванні виникає необхідність змінити регістр у введеному рядку. Наприклад, коли даний рядок повинен бути відформатованим у нижньому регістрі для подальшої обробки. У таких випадках, використання функції конвертації рядка в нижній регістр є дуже корисним і зекономить час та зусилля.

# Як

Для конвертації рядка у нижній регістр у мові Kotlin використовується функція [toLowerCase()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html). Давайте подивимося на приклад коду, щоб краще зрозуміти, як ця функція працює.

```Kotlin
fun main() {
    val str = "ЦЕ РЯДОК З ВЕЛИКИМИ ЛІТЕРАМИ"
    println(str.toLowerCase())
}
```

В результаті виконання цього коду, ми отримаємо рядок "це рядок з великими літерами", що був конвертований у нижній регістр.

# Deep Dive

Конвертація рядків у нижній регістр в мові Kotlin відбувається за допомогою функції [Unicode extensions](https://unicode.org/versions/Unicode13.0.0/ch04.pdf). Ці розширення визначають правила перетворення символів у верхній та нижній регістр. Функція toLowerCase() використовує ці розширення для конвертації рядка.

Наприклад, буква "І" має код [U+0406](https://unicode-table.com/uk/0406/), а буква "і" - [U+0456](https://unicode-table.com/uk/0456/). Згідно з правилами Unicode, під час конвертації рядка, буква "І" буде замінена на букву "і".

# Дивись також

- [Документація мови Kotlin](https://kotlinlang.org/docs/home.html)
- [Розширення Unicode](https://unicode.org/versions/)
- [Конвертація рядків у Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [Unicode таблиці](https://unicode-table.com/)