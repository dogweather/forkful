---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Wyszukiwanie i zamiana tekstu to proces lokalizacji i modyfikacji określonych ciągów znaków w tekście. Programiści robią to, aby manipulować danymi lub dostosować wydajność programu.

## Jak to zrobić:
W Kotlinie możemy używać metody `replace()` do wyszukiwania i zastępowania tekstu. Spójrz na niżej podany kod:

```Kotlin
val tekst = "Kocham Kotlin, Kotlin jest wspaniały!"
val nowyTekst = tekst.replace("Kotlin", "programowanie")

println(nowyTekst)
```
Na wyjściu otrzymamy:

```Kotlin
Kocham programowanie, programowanie jest wspaniały!
```
`replace()` wyszukuje wszystkie wystąpienia "Kotlin" i zastępuje je "programowanie".

## Deep Dive:
Historia wyszukiwania i zastępowania tekstu to historia komputerów. Od czasów, gdy komputery miały zaledwie kilka bajtów pamięci, programiści znaleźli sposób na manipulowanie tekstami. 

Co ciekawe, Kotlin oferuje alternatywę do `replace()`, metodę `replaceFirst()`, która zastępuje tylko pierwsze wystąpienie ciągu. 

Szczegóły implementacji `replace()` zależą od implementacji konkretnej JVM. Kotlin jest na tyle elastyczny, że pozwala na korzystanie z natywnych metod Java, co daje programistom dużą kontrolę nad detalami implementacji.

## Zobacz też:
1. Dokumentacja Kotlin: [replace](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
2. Dokumentacja Kotlin: [replaceFirst](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace-first.html)
3. Głębokie zrozumienie JVM i jej wpływ na funkcje Kotlin: [JVM Deep Dive](https://www.baeldung.com/jvm-architecture)