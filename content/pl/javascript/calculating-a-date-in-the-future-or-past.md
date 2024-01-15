---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Javascript: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przyszłości lub przeszłości może być przydatne podczas tworzenia różnych aplikacji, takich jak kalendarze, planery lub prognozy pogody. Poza tym, jest to również interesujące wyzwanie dla programistów, gdyż wymaga ono użycia różnych funkcji i metod w języku Javascript.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości w języku Javascript, możemy użyć wbudowanej metody `setDate()` obiektu `Date`. Poniżej przedstawiamy kod, który pokazuje, jak obliczyć datę 7 dni w przód od obecnej daty:

```Javascript
let currentDate = new Date(); // Tworzy obiekt reprezentujący bieżącą datę
currentDate.setDate(currentDate.getDate() + 7); // Dodaje 7 dni do bieżącej daty
console.log(currentDate); // Wypisuje datę 7 dni w przód
```

Powyższy kod wypisze następującą datę: Sat Aug 07 2021 00:00:00 GMT+0200 (Central European Summer Time). Podobnie, aby obliczyć datę w przeszłości, możemy użyć metody `setDate()` z ujemną wartością. Na przykład, jeśli chcemy obliczyć datę 3 dni wstecz, oto jak będzie wyglądał kod:

```Javascript
let currentDate = new Date();
currentDate.setDate(currentDate.getDate() - 3);
console.log(currentDate);
```

Kod ten wypisze: Wed Jul 28 2021 00:00:00 GMT+0200 (Central European Summer Time). Jak widać, mamy wiele możliwości manipulowania datami w Javascript, dzięki czemu możemy dostosować je do naszych potrzeb.

## Głębsze zagłębienie

Język Javascript posiada wiele wbudowanych funkcji i metod, które ułatwiają manipulowanie datami. Oto kilka przydatnych funkcji, które mogą się przydać podczas obliczania dat w przyszłości lub przeszłości:

- `getFullYear()` - zwraca rok dla danej daty
- `getMonth()` - zwraca numer miesiąca (licząc od 0) dla danej daty
- `getDate()` - zwraca dzień miesiąca dla danej daty
- `getDay()` - zwraca dzień tygodnia dla danej daty (licząc od 0)
- `getHours()` - zwraca godzinę dla danej daty
- `getMinutes()` - zwraca minuty dla danej daty
- `getSeconds()` - zwraca sekundy dla danej daty

Wszystkie te funkcje i wiele innych mogą być użyte w połączeniu z metodą `setDate()` do dokładniejszego obliczania dat.

## Zobacz również

- [Dokumentacja języka Javascript](https://developer.mozilla.org/pl/docs/Web/JavaScript)
- [Wprowadzenie do obiektu Date w Javascript](https://www.w3schools.com/js/js_dates.asp)
- [Praktyczne zastosowania obiektu Date w Javascript](https://javascript.plainenglish.io/practical-uses-of-date-object-in-javascript-ff86d61bd611)