---
title:    "Javascript: Konwertowanie daty na ciąg znaków"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie daty na ciąg znaków jest niezwykle ważną czynnością w programowaniu. Wiele systemów i aplikacji wymaga od nas sprawdzenia i wyświetlenia aktualnej daty, a często konieczne jest także przechowywanie jej w postaci stringu. Dlatego warto znać ten proces i umieć go zaimplementować w swoich projektach.

## Jak to zrobić

```javascript
// Utworzenie obiektu daty
const date = new Date();

// Utworzenie zmiennej przechowującej format daty
const options = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' };

// Wykorzystanie metody toLocaleString() do konwersji daty na string
const dateString = date.toLocaleString('pl-PL', options);

// Wyświetlenie wyniku w konsoli
console.log(dateString); // "niedziela, 15 sierpnia 2021"
```

W powyższym przykładzie wykorzystujemy obiekt daty, który jest wbudowany w język Javascript. Następnie tworzymy zmienną `options`, która przechowuje informacje o tym, w jakim formacie chcemy wyświetlić datę. W tym przypadku, wybieramy długi dzień tygodnia, pełny rok, nazwę miesiąca oraz dzień miesiąca. Na koniec, wykorzystujemy metodę `toLocaleString()` do konwersji daty na string w wybranym przez nas formacie. 

## Głębsza analiza

Istnieją różne sposoby konwertowania daty na string w języku Javascript. Jednym z nich jest wykorzystanie konstruktora `Date()` lub metody `toString()`. W przypadku konstruktora, przyjmuje on datę jako argument, a następnie zwraca string w formacie "Dziedzień Miesiąc Rok Godzina:Minuta: Sekunda". Dzięki temu możemy mieć kontrolę nad wyświetlaną datą, jednak wymaga to trochę więcej kodu. 

Natomiast metoda `toString()`zwraca datę w formacie ISO, czyli "Rok-Miesiąc-DzieńTgodzina:Minuta: Sekunda.Godzina". Ta metoda może być przydatna, jeśli konieczne jest przekazywanie daty pomiędzy różnymi systemami w jednym standardzie.

## Zobacz także

- [Dokumentacja Javascript - Date](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - JavaScript Date Formats](https://www.w3schools.com/js/js_date_formats.asp)
- [GeeksforGeeks - JavaScript toString() Method](https://www.geeksforgeeks.org/javascript-tostring-method/)