---
title:                "Interpolacja ciągu znaków"
html_title:           "C++: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Interpolacja ciągów w Kotlinie polega na wstawianiu wartości zmiennych bezpośrednio do ciągów. Programiści używają jej, aby zwiększyć czytelność i utrzymanie kodu.

## Jak to zrobić:

```Kotlin
fun main() {
   val imie = "Jan"
   val wiek = "22"
   
   println("Moje imię to $imie, a mój wiek to $wiek lat.")
}
```
Wyjście to: "Moje imię to Jan, a mój wiek to 22 lat."

## Głębsza analiza

Interpolacja ciągów jest funkcją dostępną w wielu językach programowania, choć z różnym składnią. W Kotlinie korzystamy ze znaku `$` do interpolacji. Alternatywą dla interpolacji jest konkatenacja ciągów, ale jest to mniej czytelne i może być bardziej kosztowne pod względem wydajności. Co do szczegółów implementacji, Kotlin konwertuje interpolowane ciągi na konkatenację ciągów podczas kompilacji.

## Zobacz też

[Interpolacja ciągów w Kotlinie - Dokumentacja Oficjalna](https://kotlinlang.org/docs/whatsnew12.html#string-interpolation)

[Tekst ciągów - Kotlin Programming Language](https://kotlinlang.org/docs/reference/basic-types.html#string-literals)

[Porównanie wydajności interpolacji ciągów i konkatenacji w Kotlinie](https://medium.com/@krpiotrek/string-interpolation-vs-concatenation-3ae7922b491e)