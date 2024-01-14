---
title:                "Javascript: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest niezbędnym elementem wielu aplikacji internetowych. Dzięki tej funkcji użytkownik może wyświetlić datę w formacie, który jest mu najwygodniejszy. W tym artykule dowiesz się, jak w prosty sposób przekształcić datę w ciąg znaków za pomocą języka JavaScript.

## Jak to zrobić

Aby przekonwertować datę na ciąg znaków w języku JavaScript, będziemy korzystać z metody ```toLocaleDateString()```. Poniżej przedstawione są przykładowe kody dla poszczególnych formatów daty oraz ich wyjścia:

```JavaScript
// Format daty DD/MM/RRRR
var dzisiaj = new Date();
var data = dzisiaj.toLocaleDateString('pl-PL', {day: '2-digit', month: '2-digit', year: 'numeric'});
console.log(data); // 16.01.2020

// Format daty RRRR-MM-DD
var dzisiaj = new Date();
var data = dzisiaj.toLocaleDateString('pl-PL', {year: 'numeric', month: '2-digit', day: '2-digit'});
console.log(data); // 2020-01-16

// Format daty Dzień Tygodnia, DD miesiąc RRRR
var dzisiaj = new Date();
var data = dzisiaj.toLocaleDateString('pl-PL', {weekday: 'long', day: 'numeric', month: 'long', year: 'numeric'});
console.log(data); // Czwartek, 16 stycznia 2020
```

## Głębsza analiza

Metoda ```toLocaleDateString()``` przyjmuje dwa argumenty: pierwszy to język, dla którego chcemy wyświetlić datę, a drugi to obiekt z właściwościami dotyczącymi formatowania daty. W tym drugim argumencie możemy wybrać, które elementy daty chcemy uwzględnić oraz w jakiej kolejności.

W powyższych przykładach użyliśmy kodu kraju "pl-PL" (polski dla Polski) jako pierwszego argumentu, ale można również użyć kodu kraju odpowiadającego językowi użytkownika aplikacji, co pozwala na wyświetlanie daty w odpowiednim dla niego formacie.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o przekształcaniu daty w inne typy danych lub o innych funkcjach języka JavaScript, zapoznaj się z poniższymi artykułami:

- [Konwersja daty na liczbę w języku JavaScript](https://pl.programiz.com/javascript/date-number)
- [Formatowanie daty w języku JavaScript](https://pl.freecodecamp.org/news/wygladaj-swoje-daty-praktycznie-ukazanie-czasu-okreslonego-na-stronie-internetowej-w-javascript-fb55e344177c/)
- [Dokumentacja metody toLocaleDateString() w języku JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)

Dzięki tym wskazówkom będziesz w stanie w pełni wykorzystać potencjał konwersji daty na ciąg znaków w swoich projektach.