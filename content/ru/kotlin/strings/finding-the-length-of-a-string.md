---
title:                "Поиск длины строки"
aliases:
- /ru/kotlin/finding-the-length-of-a-string.md
date:                  2024-01-28T23:57:51.342417-07:00
model:                 gpt-4-0125-preview
simple_title:         "Поиск длины строки"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/kotlin/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Нахождение длины строки означает подсчёт её символов. Программисты делают это для валидации ввода, перебора символов или выделения памяти.

## Как это сделать:
```kotlin
fun main() {
    val greeting = "Привет, мир!"
    println(greeting.length)  // печатает 12
}
```
Вывод:
```
12
```

## Подробнее
В ранние дни вычислений строки обрабатывались иначе, часто с использованием массивов с нулевым завершением в языках, таких как C. Kotlin, как современный язык, предоставляет встроенное свойство `length` для объектов String.

Альтернативы? Ну, можно было бы перебирать строку и считать символы—но зачем изобретать велосипед? `length` в Kotlin эффективно и просто.

Под капотом `length` возвращает количество единиц кода UTF-16 в строке. Это означает, что для большинства текстов (например, на английском), количество единиц кода совпадает с количеством символов. Однако для символов, выходящих за пределы Основной Многоязычной Плоскости (BMP), которые представлены двумя единицами кода (парой суррогатов), свойство `length` может не соответствовать количеству точек кода Unicode.

## Смотрите также
- Справочник по стандартной библиотеке Kotlin для строк: [Строки Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- Понимание UTF-16 и представления символов: [Unicode в Java](https://docs.oracle.com/javase/tutorial/i18n/text/unicode.html)
- Подробное исследование обработки строк в Kotlin и связанных функций: [Kotlin для разработчиков Java](https://www.coursera.org/learn/kotlin-for-java-developers)
