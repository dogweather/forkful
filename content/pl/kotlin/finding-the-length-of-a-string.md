---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Znalezienie długości ciągu znaków polega na obliczeniu liczby znaków w danym ciągu. Programiści robią to z różnych powodów, na przykład do walidacji danych wejściowych lub manipulowania ciągami znaków.

## Jak to zrobić:
Oto przykładowy kod, który pokazuje, jak możesz znaleźć długość ciągu znaków w Kotlinie:
```Kotlin
fun main(args: Array<String>) {
    val str = "Hello, Kotlin!"
    println("Długość ciągu to: ${str.length}")
}
```
Wynik tego programu to:
```
Długość ciągu to: 14
```

## Głębsze spojrzenie:
Znalezienie długości ciągu znaków jest fundamentalną operacją w programowaniu. Zostało to wprowadzone wraz z pierwszymi językami programowania i jest obecne w większości współczesnych języków, w tym w Kotlinie.

Alternatywą dla użycia funkcji .length do znalezienia długości ciągu znaków może być ręczne zliczanie znaków, choć jest to zazwyczaj mniej efektywne i bardziej skomplikowane.

Szczegół realizacji: W Kotlinie, właściwość .length dla ciągu znaków zwraca liczbę znaków Unicode 16-bit w ciągu, a nie rzeczywistą liczbę znaków, co jest ważne do zapamiętania, gdy pracujemy z ciągami zawierającymi znaki poza zestawem ASCII.

## Zobacz też:
Możesz zapoznać się z większą ilością przykładów i szczegółów na temat znalezienia długości ciągu znaków w Kotlinie, odwiedzając te strony:
- [Dokumentacja Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/length.html)
- [Przewodnik Programowania Kotlin](https://www.programiz.com/kotlin-programming/string)
- [Kotlin Tutorial - String Length](https://www.tutorialkart.com/kotlin/string-length-in-kotlin/)