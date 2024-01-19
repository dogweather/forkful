---
title:                "Знаходження довжини рядка"
html_title:           "Arduino: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Визначення довжини рядка - це процес знаходження кількості символів у рядку. Програмісти роблять це, щоб контролювати та маніпулювати даними, які вони обробляють.

## Як зробити:

У Kotlin розмір рядка визначається за допомогою властивості `length`. Ви можете побачити, як це працює, на наступному прикладі:

```Kotlin
fun main() {
    val str = "Привіт, Україно!"
    println(str.length)
}
```

Виконуючи цей код, ви отримаєте наступний результат:

```
16
```

## Глибше занурення

1. Історичний контекст: Властивість `length` є стандартною особливістю більшості мов програмування.
2. Альтернативи: У Kotlin є додаткова можливість отримати довжину рядка за допомогою функції `count()`. Ось як вона виглядає: 
```Kotlin
fun main() {
    val str = "Привіт, Україно!"
    println(str.count())
}
```
Виконання цього коду також даст вам результат 16.
3. Деталі реалізації: Функція `length` кілька швидша за `count()`, особливо для довгих рядків, бо вона повертає вже обчислену властивість, тоді як `count()` перебирає всі символи рядка.

## Дивіться також

- Документацію Kotlin про рядки: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- Корисні приклади роботи з рядками в Kotlin: [https://www.programiz.com/kotlin-programming/string](https://www.programiz.com/kotlin-programming/string)