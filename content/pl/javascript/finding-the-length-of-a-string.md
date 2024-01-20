---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?

Znalezienie długości ciągu polega na ustaleniu liczby znaków w tym ciągu. Programiści robią to, szczególnie kiedy muszą przetwarzać, zmieniać lub walidować dane tekstowe.

## Jak to zrobić:

Znalezienie długości ciągu w JavaScriptu jest proste. Wygląda to tak:

```Javascript
let tekst = "Programowanie w JavaScript";
console.log(tekst.length);
```

Gdy uruchomimy powyższy kod, otrzymamy:

```Javascript
25
```

Co oznacza, że ciąg zawiera 25 znaków.

## Dogłębna analiza

Za koncept długości ciągu stoją historyczne decyzje o implementacji. Wcześniejsze języki programowania, jak C, wymagały ręcznego zliczania znaków, podczas gdy JavaScript i inne nowoczesne języki uproszczały tę operację za pomocą wbudowanych właściwości.

Alternatywą do użycia `length` jest iterowanie przez ciąg i zliczanie znaków. Ale to jest dłuższe i bardziej skomplikowane, dlatego właściwość `length` jest zdecydowanie preferowana.

Nie musisz się martwić szczegółami implementacji `length` w JavaScript. Jest to natywna funkcja systemu, która jest zoptymalizowana do szybkiego zliczania znaków, nawet w bardzo długich ciągach.

## Zobacz także:

Szczegółowe informacje na temat operacji na ciągach w JavaScript znajdziesz na stronach:

- [MDN Web Docs - Ciąg znaków](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/String)
- [W3Schools - JavaScript String length Property](https://www.w3schools.com/jsref/jsref_length_string.asp) 

Harmonogram szkoleń oraz kursy, w których omawiane są podobne tematy znajdują się na stronach:

- [Codecademy - JavaScript](https://www.codecademy.com/learn/introduction-to-javascript)