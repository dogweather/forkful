---
title:                "Łączenie łańcuchów znaków"
html_title:           "Kotlin: Łączenie łańcuchów znaków"
simple_title:         "Łączenie łańcuchów znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## O co chodzi?

Łączenie napisów, znane też jako konkatenacja, jest powszechną operacją w programowaniu. Polega ona na łączeniu dwóch lub więcej napisów w jeden większy napis. Programiści często wykonują tę operację, aby tworzyć bardziej czytelny i dynamiczny kod.

## Jak to zrobić:

Konkatenacja w języku Kotlin jest bardzo prosta i intuicyjna. Możemy tego dokonać przy użyciu operatora „+” lub wykorzystując funkcję `plus()`.

```Kotlin
// przykład użycia operatora
val imie = "Anna"
val nazwisko = "Kowalska"
val pelneImie = imie + " " + nazwisko // wyświetli "Anna Kowalska"

// przykład użycia funkcji plus()
val tekst1 = "Hello"
val tekst2 = "World"
val wynik = tekst1.plus(" ").plus(tekst2) // wyświetli "Hello World"
```

## Głębsze zagadnienia:

Historia konkatenacji sięga początków programowania, gdy napisy były jedynym sposobem na reprezentację tekstu w komputerze. W języku Kotlin, operacja łączenia napisów jest wydajna, ponieważ zawsze tworzony jest nowy napis zamiast zmieniać istniejący. Alternatywnym sposobem na konkatenację jest wykorzystanie klasy `StringBuilder`, która jest używana do budowania i modyfikowania napisów.

## Zobacz też:

- [Dokumentacja języka Kotlin](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Porównanie wydajności konkatenacji w Kotlin i Java](https://medium.com/@lushnikov/benchmark-string-concatenation-in-java-kotlin-groovy-scala-clojure-89b970d5a00b)