---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:47.713194-07:00
description: "Jak to zrobi\u0107: W Google Apps Script mo\u017Cesz generowa\u0107\
  \ losowe liczby za pomoc\u0105 funkcji `Math.random()`, podobnie jak w JavaScript.\
  \ Funkcja ta zwraca\u2026"
lastmod: '2024-03-13T22:44:34.897998-06:00'
model: gpt-4-0125-preview
summary: "W Google Apps Script mo\u017Cesz generowa\u0107 losowe liczby za pomoc\u0105\
  \ funkcji `Math.random()`, podobnie jak w JavaScript."
title: Generowanie liczb losowych
weight: 12
---

## Jak to zrobić:
W Google Apps Script możesz generować losowe liczby za pomocą funkcji `Math.random()`, podobnie jak w JavaScript. Funkcja ta zwraca zmiennoprzecinkową, pseudolosową liczbę w zakresie od 0 (włącznie) do 1 (wyłącznie). Aby dostosować te liczby do różnych przypadków użycia, takich jak generowanie liczb całkowitych w określonym zakresie, możesz potrzebować dodatkowych obliczeń.

### Generowanie podstawowej losowej liczby
Aby wygenerować prostą losową liczbę i zalogować ją do konsoli:

```javascript
function generateRandomNumber() {
  var randomNumber = Math.random();
  Logger.log(randomNumber);
}
```
*Przykładowe wyjście:* `0.1234567890123456`

### Generowanie liczby całkowitej w określonym zakresie
Aby wygenerować losową liczbę całkowitą między dwiema wartościami (`min` i `max`), włącznie:

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  var randomNumber = Math.floor(Math.random() * (max - min + 1)) + min;
  Logger.log(randomNumber);
  return randomNumber;
}

// Przykład:
getRandomInt(1, 10);
```
*Przykładowe wyjście*: `7`

Pamiętaj, funkcja `Math.ceil()` jest używana do zaokrąglenia wartości minimalnej w górę, a `Math.floor()` jest używana do zaokrąglenia wartości maksymalnej w dół, co zapewnia, że losowa liczba jest w określonym zakresie.

## Szczegółowa analiza
Mechanizm generowania losowych liczb w Google Apps Script, a także w większości języków programowania, wykorzystuje generator pseudolosowych liczb (PRNG). Technika ta jest deterministyczna i opiera się na początkowej wartości, znanej jako ziarno, do wytworzenia sekwencji liczb, które wydają się być losowe. Choć jest to wystarczające dla wielu aplikacji, ważne jest, aby pamiętać, że liczby pseudolosowe mogą nie być odpowiednie tam, gdzie wymagane jest wysokie bezpieczeństwo lub prawdziwa losowość, takie jak w aplikacjach kryptograficznych.

Prawdziwą losowość można osiągnąć za pomocą sprzętowych generatorów liczb losowych lub usług generujących losowość z naturalnych zjawisk. Jednakże dla większości codziennych potrzeb skryptowych w Google Apps Script, `Math.random()` wystarcza.

Historycznie, poszukiwania bardziej efektywnych technik generowania liczb losowych doprowadziły do rozwoju różnych algorytmów, z których godne uwagi są Mersenne Twister i Generator Liniowo-Kongruentny (LCG). Niemniej jednak, biorąc pod uwagę wysoki poziom abstrakcji w Google Apps Script, większość użytkowników nie będzie musiała bezpośrednio implementować tych algorytmów, jednak zrozumienie leżących u ich podstaw zasad może pomóc w docenieniu znaczenia i ograniczeń generowania losowych liczb w twoich skryptach.
