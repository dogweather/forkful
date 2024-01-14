---
title:    "Javascript: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego
Osoby programujące często potrzebują wyliczyć datę w przyszłości lub w przeszłości, na przykład dla tworzenia aplikacji kalendarza lub do obsługi wydarzeń cyklicznych.

## Jak to zrobić
```Javascript
// Wyliczenie daty 30 dni w przyszłości od aktualnej daty
const now = new Date();
const futureDate = new Date(now.setDate(now.getDate() + 30));
console.log(futureDate);

// Wyliczenie daty 60 dni w przeszłości od aktualnej daty
const pastDate = new Date(now.setDate(now.getDate() - 60));
console.log(pastDate);
```
Przykładowy output:
```
Thu Dec 19 2019 21:33:32 GMT+0100 (czas środkowoeuropejski standardowy)
Sat Nov 30 2019 21:33:32 GMT+0100 (czas środkowoeuropejski standardowy)
```

## Głębsze zanurzenie
Aby wyliczyć datę w przyszłości lub przeszłości, warto wykorzystać wbudowane w Javascript funkcje Date() oraz setDate(). Istnieją również biblioteki, takie jak moment.js, które ułatwiają operacje na datach. Ważne jest też pamiętanie o sposobie działania przesuwania dat w przód i wstecz, ponieważ można łatwo popełnić błąd i otrzymać niewłaściwy wynik.

## Zobacz też
- [Dokumentacja Javascript Date()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)