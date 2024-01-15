---
title:                "Wyciąganie podciągów"
html_title:           "Swift: Wyciąganie podciągów"
simple_title:         "Wyciąganie podciągów"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu musimy odwołać się tylko do części tekstu z całego ciągu znaków. Na przykład, możemy chcieć wyodrębnić imię z adresu e-mail lub numer telefonu z tekstu. W takich sytuacjach wykorzystujemy funkcję wyodrębniania podciągów w języku Swift.

## Jak to zrobić

Aby wyodrębnić podciągi w języku Swift, możemy skorzystać z metody ```substring```, która jest dostępna dla typu ```String```. Przykładowo, jeśli mamy ciąg znaków "Hello World!", możemy wyodrębnić tylko słowo "World" w następujący sposób:

```Swift
let text = "Hello World!"
let extractedSubstring = text.substring(from: 6, to: 10)
print(extractedSubstring) // wydrukuje "World"
```

Funkcja ```substring``` przyjmuje dwa parametry: początkowy indeks i końcowy indeks podciągu. Indeksy liczymy od 0, więc w przykładzie powyżej, zaczynamy od indeksu 6, który odpowiada literze "W" w wyrazie "World", a kończymy na indeksie 10, który odpowiada ostatniej literze "d". Wykorzystując tę samą metodę, możemy także wyodrębnić podciągi z tylu ciągów znaków jak np. numerów telefonów czy adresów e-mail.

## Głębszy zanurzenie

Podczas wyodrębniania podciągów w języku Swift, musimy pamiętać o kilku ważnych rzeczach. Po pierwsze, metoda ```substring``` nie modyfikuje oryginalnego ciągu znaków, a jedynie zwraca nowy podciąg jako wynik. Po drugie, indeksy podciągu muszą być zawsze w zakresie od 0 do długości oryginalnego ciągu znaków.

Istnieją również inne metody do ekstrakcji podciągów w języku Swift, takie jak ```prefix``` i ```suffix```, które pozwalają nam na wyodrębnienie podciągów od początku i końca ciągu znaków. Możemy także wykorzystać opcjonalne parametry, takie jak np. ```lowercased``` lub ```uppercased```, aby zwrócić podciąg w określonym formacie (małych lub wielkich liter).

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o wyodrębnianiu podciągów w języku Swift, zapoznaj się z oficjalną dokumentacją języka: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#//apple_ref/doc/uid/TP40014097-CH7-ID297

Jeśli natomiast jesteś początkującym programistą w języku Swift, zobacz nasz artykuł o podstawach tego języka: [Wprowadzenie do programowania w języku Swift](https://example.com)