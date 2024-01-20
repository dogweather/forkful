---
title:                "Переведення рядка в верхній регістр"
html_title:           "Kotlin: Переведення рядка в верхній регістр"
simple_title:         "Переведення рядка в верхній регістр"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що та для чого?
Коли ми говоримо про "капіталізацію рядка", маємо на увазі перетворення першої букви рядка на велику. Пограмісти роблять це для покращення зручності читання тексту, правильного форматування виводу, або тонкої роботи з даними.

## Як це зробити:
Для капіталізації рядка в Kotlin, використовуйте метод `capitalize()`. Розглянемо приклад.

```Kotlin
fun main() {
    val helloWorld = "вітаю світ"
    val capitalizedHelloWorld = helloWorld.capitalize()

    println(capitalizedHelloWorld)  // Output: "Вітаю світ"
}
```
В даному прикладі, `capitalize()` змінив першу літеру рядка helloWorld на велику і вивели це за допомогою `println()`. 

## Поглиблений огляд
Функцію `capitalize()` було вперше введено в Kotlin 1.0 для того, щоб програмісти могли легко працювати з текстовими даними. Хоча `capitalize()` є вельми зручною, вона має альтернативу в Kotlin 1.5 і вище - `replaceFirstChar()`. Суть використання та ж, але ви можете задати більш складну логіку перетворення першого символу.

```Kotlin
fun main() {
    val helloWorld = "вітаю світ"
    val capitalizedHelloWorld = helloWorld.replaceFirstChar{ if (it.isLowerCase()) it.titlecase() else it.toString() }

    println(capitalizedHelloWorld)  // Output: "Вітаю світ"
}
```

У багатьох випадках ви можете використовувати будь-яку з цих функцій, але важливо знати про наявність обох і розуміти різницю.

## Див. також
Для більш детального вивчення можете використати такі джерела:
- [capitalize() документація Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [replaceFirstChar() документація Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace-first-char.html)
- [Офіційна документація Kotlin](https://kotlinlang.org/docs/home.html)

Програмуйте з насолодою!