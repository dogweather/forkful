---
title:                "Zmiana tekstu na wielkie litery"
html_title:           "Kotlin: Zmiana tekstu na wielkie litery"
simple_title:         "Zmiana tekstu na wielkie litery"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?

W programowaniu, "kapitalizacja" oznacza zmianę pierwszej litery w zdaniu na dużą literę. Programiści często to robią, aby poprawić czytelność tekstu lub aby spełnić pewne wymagania w systemach informatycznych.

## Jak to zrobić:

Kotlin ma wbudowaną funkcję, która umożliwia kapitalizację tekstu. Aby tego dokonać, wystarczy użyć funkcji "capitalize()" na zmiennej typu String.

```Kotlin
val text = "witaj, świecie"
println(text.capitalize())
// Output: Witaj, świecie
```

## Głębszy zanurzenie:

Niektórzy programiści stosują również inne sposoby na kapitalizację tekstu, np. zmieniając wszystkie litery na wielkie lub korzystając z różnych bibliotek zewnętrznych. Jednak wbudowana funkcja "capitalize()" w Kotlin jest prostym i wygodnym sposobem na to.

## Zobacz też:

- Dokumentacja Kotlin o funkcji "capitalize()": https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html