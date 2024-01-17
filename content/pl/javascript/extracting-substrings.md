---
title:                "Ekstrakcja podciągów"
html_title:           "Javascript: Ekstrakcja podciągów"
simple_title:         "Ekstrakcja podciągów"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Wyciąganie podciągów, czyli fragmentów tekstu, jest jedną z podstawowych operacji, którą programiści wykonują na łańcuchach znaków. Dzięki temu można wyodrębnić wybrane części tekstu, co jest szczególnie przydatne w przypadku analizowania lub przetwarzania danych.

## Jak to zrobić:
```Javascript
// Przykładowy łańcuch znaków
let tekst = "Jestem programistą Javascript";

// Wyodrębnianie podciągu od indeksu 8
let podciag = tekst.substring(8);
console.log(podciag); // "programistą Javascript"

// Wyodrębnianie podciągu z przedziału indeksów
let podciag2 = tekst.substring(8, 18);
console.log(podciag2); // "programistą"

// Wyodrębnianie podciągu na podstawie indeksów znaków
let podciag3 = tekst.substring(tekst.indexOf("J"), tekst.indexOf("programistą") + 11);
console.log(podciag3); // "Jestem programistą"

// Wyodrębnianie ostatnich znaków
let podciag4 = tekst.substring(tekst.length - 10);
console.log(podciag4); // "Javascript"
```

## Głębsza analiza:
Wyciąganie podciągów jest dostępne w większości języków programowania, jednak w Javascript istnieje również alternatywna metoda - metoda slice(). Różnica między nimi polega na tym, że metoda substring() przyjmuje także indeksy ujemne, co oznacza, że liczenie odbywa się od końca tekstu. Należy również pamiętać, że metoda substring() nie modyfikuje oryginalnego łańcucha, a zwraca nowy.

## Zobacz także:
[Metoda substring() w dokumentacji Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)