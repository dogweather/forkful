---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wyciąganie podciągów to działanie polegające na pobieraniu części tekstu z większego ciągu znaków. Programiści robią to, aby manipulować wystarczająco małymi częściami danych tekstowych, które są istotne dla konkretnej operacji.

## Jak to zrobić:

Dzięki Kotlin, łatwo jest to zrobić. Oto przykładowy kod:

```kotlin
val str = "Witaj, świecie Kotlin!"
val substr = str.substring(0, 5)
println(substr) // Wydrukuje: Witaj
```

Tutaj mamy funkcję `substring`, która przyjmuje dwa argumenty: indeks początkowy i końcowy. Działa ona poprzez wyodrębnienie fragmentu tekstu od indeksu początkowego do końcowego (końcowy indeks jest wyłączony).

## Deep Dive

Doszło do tego, że "wyciąganie podciągów" to koncept, który znajduje się w prawie każdym języku programowania, oznacza to, że jest to podstawowe działanie przy manipulowaniu tekstem. Alternatywnie, w Kotlinie można również korzystać z funkcji `slice`:

```kotlin
val str = "Witaj, świecie Kotlin!"
val substr = str.slice(0..4)
println(substr) // Wydrukuje: Witaj
```

Implementacja `substring` i `slice` jest prawie identyczna, rozwijająca się w taki sam sposób. Różnica polega na tym, że `slice` zwraca listę znaków, podczas gdy `substring` zwraca ciąg znaków.

## Zobacz także:

1. Dokumentacja Kotlin na temat manipulacji tekstami: [Kotlin Text](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/)
2. Porównanie między `substring` a `slice`: [Kotlin slice vs substring](https://stackoverflow.com/questions/57094803/what-is-the-difference-between-substring-and-slice-in-kotlin)
3. Sprawdzenie i poczęcie otrzymywania podciągów: [Tutorial: Konwersja, suma, odbić i porównać ciągi, uzyskiwać podciągi](https://kotlinlang.org/docs/idioms.html#strings)