---
title:                "Zamiana tekstu na wielkie litery"
html_title:           "C: Zamiana tekstu na wielkie litery"
simple_title:         "Zamiana tekstu na wielkie litery"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zmienianie wszystkich liter w napisie na duże, zwane też kapitalizacją, to proste zadanie w programowaniu. Programiści używają tej techniki, by na przykład ułatwić użytkownikom czytanie wyjścia programu.

## Jak to zrobić:

Podajemy tu kod z użyciem standardowej biblioteki `ctype.h` w wersji C99:

```C
#include <ctype.h>
#include <stdio.h>

void naDuze(char *napis) {
    while(*napis) {
        *napis = toupper((unsigned char) *napis);
        napis++;
    }
}

int main() {
    char napis[] = "dzień dobry, panie programisto!";
    naDuze(napis);
    printf("%s\n", napis); 
    // Wyjście: "DZIEŃ DOBRY, PANIE PROGRAMISTO!"
    return 0;
}
```

## Głębsze spojrzenie

Kapitalizacja napisów pojawiła się w bardzo wczesnych językach programowania. Bazuje ona na różnicy wartości ASCII pomiędzy dużymi a małymi literami.

Istnieją też alternatywne sposoby kapitalizacji napisów, np. używanie funkcji `toupper` bezpośrednio z biblioteki `ctype.h`.

Główne szczegóły implementacyjne polegają na przechodzeniu przez każdy znak w napisie i użyciu funkcji `toupper` na każdym znaku łanucha. Ważne jest jednak, że `toupper` działa tylko na małe litery. Dlatego też konieczne jest rzutowanie na `(unsigned char)`, aby uniknąć niezdefiniowanych zachowań.

## Zobacz też

1. Dokumentacja <ctype.h>: https://pl.wikibooks.org/wiki/C/Biblioteki_standardowe/ctype.h
2. Kod ASCII: https://pl.wikipedia.org/wiki/ASCII
3. Inne techniki na kapitalizację napisów: https://stackoverflow.com/questions/15508570/c-program-to-capitalize-first-letter-of-each-word-in-string