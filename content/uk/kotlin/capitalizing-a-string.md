---
title:                "Заголовок розділу: Капіталізація рядка"
html_title:           "Kotlin: Заголовок розділу: Капіталізація рядка"
simple_title:         "Заголовок розділу: Капіталізація рядка"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Якщо ви працюєте зі строки в своїй програмі, ви можете зіткнутись з необхідністю капітулювати її. Це може знадобитися для виведення її в заголовок або для налагодження коректного форматування тексту.

## Як

```Kotlin
val str = "котик"
val capitalizedStr = str.capitalize()
println(capitalizedStr)
```

Вивід: "Котик"

## Глибока занурення

Коли ви викликаєте функцію `capitalize()` на строці, вона повертає копію строки з першою літерою, яка була перетворена на велику. Це дає можливість не змінювати оригінальну строку, а просто працювати з копією. Також існує метод `decapitalize()`, який перетворює першу літеру на малу.

## Дивись також

- [Kotlin Docs: capitalize()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [Kotlin Docs: decapitalize()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/decapitalize.html)
- [Kotlin String Functions](https://www.baeldung.com/kotlin/string-functions)