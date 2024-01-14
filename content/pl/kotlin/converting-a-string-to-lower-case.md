---
title:                "Kotlin: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli korzystasz z języka Kotlin, prawdopodobnie już wiesz, że jest to język bardzo intuicyjny i przyjazny dla programistów. Jednym z jego wielu przydatnych funkcji jest możliwość konwertowania ciągów znaków na małe litery. W tym artykule dowiesz się, dlaczego warto wykorzystywać tę funkcję i jak ją użyć w swoich projektach.

## Jak to zrobić

Konwertowanie ciągów znaków na małe litery w Kotlinie jest bardzo proste, ponieważ język ten posiada wbudowaną metodę o nazwie "toLowerCase()". Ta metoda przyjmuje ciąg znaków i zwraca nowy ciąg zawierający wszystkie litery w małej formie.

```Kotlin
val tekst = "KOTLIN"
val tekstMalymiLiterami = tekst.toLowerCase()

println(tekstMalymiLiterami) // wypisze "kotlin" na konsolę
```

Możesz również użyć tej metody z połączeniu z metodą "replace()" do zamiany wyłącznie wielkich liter na małe.

```Kotlin
val zdanie = "To Jest Przykład"
val zdanieMalymiLiterami = zdanie.replace("[A-Z]".toRegex(), { it.value.toLowerCase() })

println(zdanieMalymiLiterami) // wypisze "to jest przykład" na konsolę
```

## Deep Dive

Podczas konwertowania ciągów znaków na małe litery, warto mieć świadomość różnic kulturowych i językowych. Metoda "toLowerCase()" działa zgodnie z zasadami gramatyki języka angielskiego, więc nie może zapewnić poprawnego działania dla innych języków. Jeśli pracujesz z kodowaniem Unicode, warto zwrócić uwagę na to, że niektóre znaki mogą zachować swoją wielkość.

## Zobacz też

- Dokumentacja w języku Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html
- Przykłady wykorzystania metody "toLowerCase()": https://www.jetbrains.com/help/kotlin/basic-types.html#strings
- Wpływ różnych języków i kultur na konwersję stringów: https://blog.caseyliss.com/2017/10/8/capitalization-insensitivity