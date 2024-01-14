---
title:    "Javascript: Uzyskiwanie aktualnej daty"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Dlaczego warto pisać w języku JavaScript?

Język JavaScript jest jednym z najpopularniejszych języków programowania na świecie. Jest on wykorzystywany w tworzeniu stron internetowych, aplikacji mobilnych oraz w wielu innych dziedzinach. Dzięki swojej prostocie i możliwości wykorzystania zarówno po stronie klienta, jak i serwera, JavaScript jest narzędziem niezbędnym dla każdego programisty. W tym artykule skupimy się na jednej z podstawowych funkcji języka - pobieraniu aktualnej daty.

## Jak pobrać aktualną datę w języku JavaScript?

Pobranie aktualnej daty w języku JavaScript jest bardzo proste i wymaga zaledwie kilku linijek kodu. W celu jej uzyskania, możemy wykorzystać obiekt Date, który reprezentuje aktualną datę i czas. Poniżej znajduje się przykładowy kod, który pozwoli nam pobrać aktualną datę:

```Javascript
let currentDate = new Date();
console.log(currentDate);
```

Po uruchomieniu tego kodu w konsoli przeglądarki, powinniśmy otrzymać aktualną datę i czas w formacie `Date {Sat Sep 25 2021 15:48:38 GMT+0200 (Central European Summer Time)}`.

Kod ten wykorzystuje wbudowaną funkcję konstruktora `new Date()`, która tworzy nowy obiekt Date, reprezentujący aktualną datę i czas. Możemy również wybrać konkretną datę i czas, podając odpowiednie parametry do funkcji konstruktora. Przykładowo, aby uzyskać datę 25 grudnia 2021 roku, możemy napisać `new Date(2021, 11, 25)` - pierwszy parametr to rok, drugi to miesiąc (licząc od 0, czyli 11 oznacza grudzień), a trzeci to dzień.

Możemy również wykorzystać różne metody wbudowane w obiekt Date, aby otrzymać poszczególne elementy daty, takie jak dzień, miesiąc czy rok. Poniżej znajduje się przykładowy kod, który pozwala na uzyskanie aktualnego dnia i miesiąca:

```Javascript
let currentDate = new Date();
let day = currentDate.getDay();
let month = currentDate.getMonth();

console.log("Dzień: " + day);
console.log("Miesiąc: " + month);
```

W powyższym przykładzie wykorzystaliśmy metody `getDay()` i `getMonth()` do pobrania odpowiednio dnia tygodnia (licząc od 0, gdzie 0 oznacza niedzielę) i miesiąca (również licząc od 0, gdzie 0 oznacza styczeń).

## Wiecej informacji o pobieraniu aktualnej daty w języku JavaScript

Obiekt Date posiada wiele innych metod, dzięki którym możemy pobierać i manipulować datami i czasami. Dokładniejszą dokumentację możemy znaleźć na oficjalnej stronie języka JavaScript. Warto również zauważyć, że nie wszystkie przeglądarki obsługują wszystkie funkcje i metody związane z obiektem Date, więc warto sprawdzić jej dostępność przed wykorzystaniem.

## Zobacz także

- [Oficjalna dokumentacja języka JavaScript](https://developer.mozilla.org/pl/docs/Web/JavaScript)
- [Wprowadzenie do języka JavaScript](https://www.w3schools.com/js/)
- [Funkcje konstruktora w obiekcie Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/Date)