---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:42.250779-07:00
description: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\
  \u0438\u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u0441\u0442\u0440\u043E\
  \u043A\u0443 \u0441 \u0437\u0430\u0433\u043B\u0430\u0432\u043D\u044B\u043C\u0438\
  \ \u0431\u0443\u043A\u0432\u0430\u043C\u0438 \u043E\u0437\u043D\u0430\u0447\u0430\
  \u0435\u0442 \u043F\u0440\u0435\u0432\u0440\u0430\u0449\u0435\u043D\u0438\u0435\
  \ \u043F\u0435\u0440\u0432\u043E\u0439 \u0431\u0443\u043A\u0432\u044B \u043A\u0430\
  \u0436\u0434\u043E\u0433\u043E \u0441\u043B\u043E\u0432\u0430 \u0432 \u0432\u0435\
  \u0440\u0445\u043D\u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\
  \u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:44.947063-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\
  \u0438\u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u0441\u0442\u0440\u043E\
  \u043A\u0443 \u0441 \u0437\u0430\u0433\u043B\u0430\u0432\u043D\u044B\u043C\u0438\
  \ \u0431\u0443\u043A\u0432\u0430\u043C\u0438 \u043E\u0437\u043D\u0430\u0447\u0430\
  \u0435\u0442 \u043F\u0440\u0435\u0432\u0440\u0430\u0449\u0435\u043D\u0438\u0435\
  \ \u043F\u0435\u0440\u0432\u043E\u0439 \u0431\u0443\u043A\u0432\u044B \u043A\u0430\
  \u0436\u0434\u043E\u0433\u043E \u0441\u043B\u043E\u0432\u0430 \u0432 \u0432\u0435\
  \u0440\u0445\u043D\u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440. \u041F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\
  \u0430\u044E\u0442 \u044D\u0442\u043E \u0434\u043B\u044F\u2026"
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u0432\u0435\u0440\u0445\u043D\
  \u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440"
---

{{< edit_this_page >}}

## Что и почему?

Преобразование строки в строку с заглавными буквами означает превращение первой буквы каждого слова в верхний регистр. Программисты делают это для форматирования текста, чтобы имена, заголовки или элементы пользовательского интерфейса выглядели аккуратно и стандартизированно.

## Как это сделать:

В Kotlin вы можете легко преобразовывать строки в строки с заглавными буквами. Вот быстрый пример:

```kotlin
fun main() {
    val text = "kotlin программирование"
    val capitalizedText = text.split(" ").joinToString(" ") { it.capitalize() }
    println(capitalizedText)
}
```

Пример вывода:
```
Kotlin Программирование
```
Чтобы сделать заглавной только первую букву предложения:

```kotlin
fun main() {
    val sentence = "привет, энтузиасты kotlin!"
    val capitalizedSentence = sentence.replaceFirstChar { if (it.isLowerCase()) it.titlecase() else it.toString() }
    println(capitalizedSentence)
}

```

Пример вывода:
```
Привет, энтузиасты kotlin!
```

Обратите внимание, что `capitalize()` устарел. Используйте `replaceFirstChar { it.titlecase() }` для лучшей совместимости в будущем.

## Подробнее

Методы преобразования в заглавные буквы изменились в Kotlin. `capitalize()` широко использовался, но был объявлен устаревшим в пользу `replaceFirstChar { it.titlecase() }`. Это изменение делает код более понятным в том, что происходит - это не просто преобразование в заглавные буквы, а замена первого символа на его эквивалент в верхнем регистре.

Почему преобразовать строки в строки с заглавными буквами? Это часто касается пользовательского интерфейса. Подумайте о названиях книг, именах или любом списке, где вам нужна последовательность. Это помогает с читаемостью и эстетикой.

Альтернативы преобразованию в заглавные буквы включают:
- `.toLowerCase()`: Для преобразования в нижний регистр.
- `.toUpperCase()`: Для преобразования всего текста в верхний регистр.
- CSS в веб-разработке: иногда текст делают заглавным на фронтенде.

Внутри функции преобразования в заглавные буквы взаимодействуют с символами Юникода. Символы имеют определенные версии в верхнем регистре. Речь идет не просто о замене 'а' на 'А', а о понимании правил, специфичных для языка.

Не забывайте о локализации. Например, в турецком 'i' преобразуется в 'İ', а не в 'I'. Таким образом, локально-агностичное преобразование может подвести вас в многоязычных приложениях.

## Смотрите также:

- Документация Kotlin по `replaceFirstChar`: [Kotlin replaceFirstChar](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace-first-char.html)
- Правила преобразования в верхний регистр Юникода: [Правила преобразования Юникода в верхний регистр](http://unicode.org/versions/Unicode9.0.0/ch03.pdf#G33992)
- Преобразование в заглавные буквы в разных локализациях: [Преобразование в зависимости от локализации](https://garygregory.wordpress.com/2015/11/03/java-lowercase-conversion-turkey/)
