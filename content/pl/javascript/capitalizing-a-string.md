---
title:                "Zmiana wielkości liter w ciągu znaków"
html_title:           "Javascript: Zmiana wielkości liter w ciągu znaków"
simple_title:         "Zmiana wielkości liter w ciągu znaków"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zdarzyło Ci się, że musiałeś pozmieniać wielkość liter w ciągu znaków? Może pracujesz nad projektem, który wymaga jednolitej pisowni, albo po prostu chcesz wyświetlić użytkownikom imię zaczynające się z dużej litery? W takich sytuacjach capitalizacja stringu może być bardzo pomocna.

## Jak to zrobić

```Javascript
// Przykładowy ciąg znaków
let imie = "jan";

// Wykorzystanie metody toUpperCase()
let imieZDuzej = imie.toUpperCase();

//Wypisanie wyniku w konsoli
console.log(imieZDuzej); // "JAN"
```

W powyższym przykładzie wykorzystaliśmy wbudowaną metodę **toUpperCase()**, która zmienia małe litery na wielkie. W ten sposób nie musimy ręcznie zmieniać wielkości liter w ciągu znaków, co jest czasochłonne i skłonne do błędów.

## Deep Dive

Metoda **toUpperCase()** jest częścią obiektu String w języku Javascript. Możemy ją wykorzystać do zmiany wielkości liter w dowolnym ciągu znaków, nie tylko w pojedynczym wyrazie. Warto również zauważyć, że istnieje również metoda **toLowerCase()**, która działa w przeciwny sposób - zamienia wielkie litery na małe.

## Zobacz także

- [Metoda toUpperCase() w dokumentacji Mozilla](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [Metoda toLowerCase() w dokumentacji Mozilla](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)