---
title:                "Zmiana ciągu znaków na małe litery"
html_title:           "Javascript: Zmiana ciągu znaków na małe litery"
simple_title:         "Zmiana ciągu znaków na małe litery"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwertowanie ciągu znaków na małe litery jest procesem zmiany każdej litery w tekście na jej odpowiednik w małym formacie. Programiści często wykonują tę operację w celu porównania dwóch ciągów znaków, ponieważ to pozwala uniknąć błędów związanych z różnicami w wielkości liter.

## Jak to zrobić:

```Javascript
let text = "PRZYKŁADOWY TEKST"; // Tworzymy zmienną z ciągiem znaków w formacie dużym
let lowerCaseText = text.toLowerCase(); // Wykonujemy konwersję na małe litery
console.log(lowerCaseText); // Wyświetlamy rezultat w konsoli: "przykładowy tekst"
```

## Głębsza analiza:

1. Kontekst historyczny: Konwersja ciągu znaków na małe litery jest częstym problemem w informatyce od czasów, gdy powstały pierwsze języki programowania. Przez lata wykształciły się różne sposoby na rozwiązanie tego problemu, jednak obecnie funkcja `toLowerCase()` jest najpopularniejszym wyborem w języku JavaScript.
2. Alternatywy: W języku JavaScript istnieje także funkcja `toUpperCase()`, która wykonuje odwrotną operację - zamienia wszystkie litery na wielkie. Istnieją również zewnętrzne biblioteki, które oferują bardziej rozbudowane funkcje zmiany wielkości liter w ciągu znaków.
3. Szczegóły implementacji: Podczas wykonywania konwersji na małe litery funkcja `toLowerCase()` korzysta z tablicy ASCII, która przypisuje każdej literze swoją wartość numeryczną. Dzięki temu możliwa jest zamiana litery z jej wartością numeryczną na jej odpowiednik w małym formacie.

## Zobacz również:

- [Funkcja toLowerCase() - dokumentacja MDN](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Konwersja znaków w języku JavaScript - artykuł na blogu Medium](https://medium.com/@nitinpatel_20236/conversions-in-javascript-c298ae64ea47)