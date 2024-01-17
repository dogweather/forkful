---
title:                "Porównywanie dwóch dat"
html_title:           "TypeScript: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## W czym rzecz & dlaczego?
Porównywanie dwóch dat jest narzędziem, które pozwala na porównanie dwóch dat ze sobą i sprawdzenie, która jest wcześniejsza lub późniejsza. Programiści często wykonują tę czynność w celu zarządzania datami w aplikacjach lub do obliczeń czasu.

## Jak to zrobić:
Oto przykład, jak porównać dwie daty w TypeScript:

```TypeScript
let data1 = new Date("2021-05-20");
let data2 = new Date("2021-05-30");

if(data1 < data2) {
  console.log("Data 1 jest wcześniejsza.");
} else if(data1 == data2) {
  console.log("Daty są takie same.");
} else {
  console.log("Data 2 jest wcześniejsza.");
}
```

**Wynik: Data 1 jest wcześniejsza.**

Kod ten tworzy dwie zmienne reprezentujące daty i porównuje je za pomocą operatorów porównania "<" i "==". W zależności od wyniku, wyświetlana jest odpowiednia wiadomość.

## Głębsza analiza:
Porównywanie dat ma swoje korzenie w programowaniu od samego początku. W starszych językach programowania, takich jak C czy Java, porównywanie dat wymagało użycia specjalnych funkcji, a czasem nawet stawiania warunków porównania dla każdego elementu daty (dzień, miesiąc, rok). Jednakże w nowszych językach, takich jak TypeScript, wystarczy użycie operatorów porównania, co ułatwia pracę programistom.

Alternatywą dla porównywania dat jest użycie biblioteki, która oferuje funkcje dedykowane do obsługi dat. Przykładem takiej biblioteki dla TypeScript jest moment.js.

Podczas wewnętrznej implementacji porównywania dat, TypeScript konwertuje daty na liczby (timestamp) i porównuje je ze sobą. Timestamp jest to liczba sekund lub milisekund, która upływa od początku epoki (1 stycznia 1970 r. w czasie UTC).

## Zobacz też:
- [Dokumentacja TypeScript](https://www.typescriptlang.org/docs/handbook/intro.html)
- [Biblioteka moment.js](https://momentjs.com/)
- [Wprowadzenie do timestampów](https://pl.wikipedia.org/wiki/Unix)