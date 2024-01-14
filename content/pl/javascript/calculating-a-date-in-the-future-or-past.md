---
title:    "Javascript: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Dlaczego?

Kalkulowanie daty w przyszłości lub przeszłości może być niezwykle przydatne w wielu różnych projektach i aplikacjach. Pozwala to na dynamiczne określanie dat i wyświetlanie ich w odpowiednich formatach, co ułatwia użytkownikom korzystanie z programu. 

## Jak to zrobić?

Aby obliczyć datę w przyszłości lub przeszłości, musimy skorzystać z wbudowanych funkcji w języku Javascript. W pierwszej kolejności, musimy zadeklarować zmienną przechowującą datę, którą chcemy manipulować. Następnie, używając funkcji `getDate()`, `getMonth()` i `getFullYear()`, możemy odczytać odpowiednio dzień, miesiąc i rok z podanej daty. Następnie, aby dodać lub odjąć odpowiednią liczbę dni, miesięcy lub lat, możemy użyć funkcji `setDate()`, `setMonth()` i `setFullYear()`. 

Poniżej przedstawione są dwa przykładowe kodu wraz z wynikami:

```Javascript
// Obliczanie daty w przyszłości
let dzisiaj = new Date();
dzisiaj.setDate(dzisiaj.getDate() + 7); // dodaje 7 dni
console.log(dzisiaj); // wyświetli datę z za 7 dni

// Obliczanie daty w przeszłości
let dzisiaj = new Date();
dzisiaj.setFullYear(dzisiaj.getFullYear() - 2); // odejmuje 2 lata
console.log(dzisiaj); // wyświetli datę sprzed 2 lat
```

## Deep Dive

Obliczanie daty w przyszłości lub przeszłości może być bardziej skomplikowane, gdy musimy uwzględnić różne formaty wyjściowe lub uwzględnić różne strefy czasowe. W takim przypadku, warto skorzystać z zewnętrznych bibliotek, takich jak `moment.js`, które umożliwiają łatwe manipulowanie datą i obsługę stref czasowych. 

W przypadku, gdy potrzebujemy obliczyć datę w przeszłości lub przyszłości od podanej przez użytkownika, musimy najpierw odpowiednio sformatować wprowadzoną przez niego datę i wykonać konwersję na obiekt typu `Date`. Następnie, za pomocą wspomnianych wcześniej funkcji, możemy dodać lub odjąć odpowiednią liczbę dni, miesięcy lub lat.

## Zobacz również

- [Dokumentacja języka Javascript wraz z opisem wbudowanych funkcji dotyczących daty](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Oficjalna strona biblioteki moment.js](https://momentjs.com/)
- [Przewodnik po manipulacji datami w Javascript](https://www.sitepoint.com/manipulating-dates-times-javascript/)