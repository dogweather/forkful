---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "TypeScript: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Obliczanie daty w przyszłości lub przeszłości to prosta koncepcja polegająca na dodawaniu do obecnej daty określonej ilości dni, miesięcy czy lat w celu uzyskania nowej daty. Programiści robią to bardzo często, na przykład do tworzenia harmonogramów lub prognoz.

## Jak to zrobić:

```TypeScript
let teraz: Date = new Date();

// Obliczanie daty 5 dni do przodu
let przyszlosc: Date = new Date();
przyszlosc.setDate(teraz.getDate() + 5);
console.log(przyszlosc);

// Obliczanie daty 5 dni wstecz
let przeszlosc: Date = new Date();
przeszlosc.setDate(teraz.getDate() - 5);
console.log(przeszlosc);
```

### Wyjście
```TypeScript
2022-03-21T00:00:00.000Z 
2022-03-11T00:00:00.000Z
```
Za pomocą metody setDate natychmiastowe dodanie lub odjęcie dni jest proste i wydajne.

## Deep Dive

Concept obliczania daty w przyszłości lub przeszłości to nie jest nową ideą i jest powszechnie stosowany w różnych dziedzinach programowania. W TypeScript, modyfikując wartość getDate(), możemy łatwo manipulować datami w przyszłości i przeszłości.

Alternatywnie, dla bardziej złożonych operacji na datach, możemy skorzystać z bibliotek takich jak Moment.js, które oferują bardziej zaawansowane funkcje manipulacji datą.

Co do szczegółów implementacyjnych, metoda setDate() działa zarówno dla przyszłych, jak i przeszłych dat i jest dostępna we wszystkich współczesnych przeglądarkach, co czyni ją bardzo użyteczną dla większości programistów TypeScript.

## Zobacz też

- [Podręcznik TypeScript: Data](https://www.typescriptlang.org/docs/handbook/basic-types.html#date)
- [Przewodnik po manipulacji datami w JavaScript](https://flaviocopes.com/how-to-date-javascript/)
- [Libraries for working with dates in TypeScript and JavaScript](https://www.smashingmagazine.com/2021/06/complete-guide-date-time-strings-javascript/)