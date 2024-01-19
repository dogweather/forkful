---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Analiza składniowa daty odnosi się do przetwarzania ciągu znaków, aby przekształcić go w obiekt daty. Programiści robią to, aby ułatwić manipulację i formatowanie dat.

## Jak to zrobić:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val stringDate = "2022-01-01"
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val date = LocalDate.parse(stringDate, formatter)

    println(date)
}
```
Kiedy uruchomisz ten kod, jako wyjście otrzymasz:

```Kotlin
2022-01-01
```

## Zagłębianie się:

Analiza składniowa daty z ciągu znaków jest praktyką powszechnie stosowaną od czasów, gdy pierwsze języki programowania zaczęły obsługiwać daty. Jest wiele alternatyw do analizy składniowej daty, takich jak bezpośrednie użycie wartości liczbowych do tworzenia dat.

W Kotlinie, do parsowania daty ze stringa, używany jest obiekt `DateTimeFormatter`. Co istotne, `DateTimeFormatter` jest niemutowalny, co oznacza, że może być bezpiecznie używany w wielowątkowości bez dodatkowej synchronizacji.

## Zobacz również:

1. Kotlin dokumentacja do DateTimeFormatter: [link](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date-time-formatter/index.html)
2. Tutorial o manipulacji datami w Kotlinie: [link](https://www.baeldung.com/kotlin/dates)
3. Podcast na temat zarządzania datami w Kotlinie: [link](https://talkingkotlin.com/dates/)