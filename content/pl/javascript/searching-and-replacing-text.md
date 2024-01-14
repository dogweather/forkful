---
title:                "Javascript: Wyszukiwanie i zamienianie tekstu"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego 

W dzisiejszym poście omówimy zagadnienie wyszukiwania i zamiany tekstu w języku JavaScript. Jest to niezwykle ważne narzędzie w programowaniu, które pozwala na szybką i precyzyjną modyfikację tekstu w naszym kodzie. Jeśli chcesz dowiedzieć się więcej o tej funkcji i jak jej używać, to ten artykuł jest dla Ciebie!

## Jak to zrobić

### Przygotowanie danych

Zanim przejdziemy do kodowania, musimy przygotować nasze dane. Załóżmy, że mamy tekst, w którym chcemy zamienić litery "a" na "e".

```javascript
let tekst = "Programowanie jest zabawa";
```

Teraz możemy przejść do właściwej części - zamiany tekstu.

### Wyszukiwanie i zamiana

W JavaScript mamy specjalną funkcję, która pozwala nam na wyszukiwanie oraz zamianę tekstu w zadanym ciągu znaków. Jest to metoda `replace()` i wygląda ona następująco:

```javascript
let nowyTekst = tekst.replace('a', 'e');
console.log(nowyTekst);
```

W wyniku dostaniemy:

```
Progrmowanie jest zebew
```

Jak widzimy, wszystkie litery "a" zostały zamienione na "e".

### Używanie wyrażeń regularnych

Możemy również użyć wyrażeń regularnych, aby wyszukać i zmienić tekst. Na przykład, jeśli chcemy zamienić wszystkie samogłoski na znaki "*", możemy użyć poniższego kodu:

```javascript
let nowyTekst2 = tekst.replace(/[aeiou]/g, '*');
console.log(nowyTekst2);
```

W wyniku dostaniemy:

```
Pr*gr*m*wn**e j*st z*b*w*
```

### Zachowanie orginalnego tekstu

Jeśli chcemy zachować oryginalny tekst i stworzyć nowy, zmodyfikowany, możemy to zrobić za pomocą składni z użyciem wyrażenia regularnego:

```javascript
let nowyTekst3 = tekst.replace(/[g]/, 'G');
console.log(nowyTekst3);
console.log(tekst); // oryginalny tekst zostaje niezmieniony
```

W wyniku dostaniemy:

```
Programowanie jest zGbawa
Programowanie jest zabawa
```

## Deep Dive

Metoda `replace()` jest wyjątkowo przydatna, ponieważ daje nam wiele możliwości. Możemy wyszukiwać konkretny tekst, wyrażenia regularne, a także użyć funkcji zwrotnej do bardziej zaawansowanego przetwarzania tekstu. Polecam prześledzić dokumentację, aby poznać wszystkie jej funkcjonalności.

## Zobacz także

- [Dokumentacja metody replace()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/replace)
- [Poradnik wyrażeń regularnych w JavaScript](https://kursjs.pl/kurs/regex/regex.php)