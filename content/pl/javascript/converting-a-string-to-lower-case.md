---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Przekształcanie napisu na małe litery w JavaScript

## Co i dlaczego?
Przekształcenie napisu na małe litery oznacza zmianę wszystkich wielkich liter w napisie na ich małe odpowiedniki. Programiści robią to, aby uniknąć pomyłek związanych z rozróżnianiem wielkości liter, co jest przydatne przy porównywaniu łańcuchów lub wyszukiwaniu danych.

## Jak to zrobić:

W JavaScript, aby przekształcić napis na małe litery, używamy metody `.toLowerCase()`. Poniżej znajduje się kod i przykładowe wyjście:

```Javascript
let greeting = 'Cześć, JAK SIĘ MASZ?';
let lowerCaseGreeting = greeting.toLowerCase();
console.log(lowerCaseGreeting);  // wydrukuje: 'cześć, jak się masz?'
```

## Dogłębne informacje

1. Historyczne kontekst: Metoda `.toLowerCase()` jest dostępna w JavaScript od jego wczesnych wersji, co czyni ją jednym z podstawowych narzędzi dla programistów.
2. Alternatywy: Choć metoda `.toLowerCase()` jest najprostszą i najczęściej używaną, można również użyć metody `.toLocaleLowerCase()`, która dodatkowo uwzględnia lokalne ustawienia językowe.
3. Szczegóły implementacji: Metoda `.toLowerCase()` nie modyfikuje oryginalnego napisu. Zamiast tego zwraca nowy napis, w którym wszystkie wielkie litery są zamienione na małe.

```Javascript
let name = 'JAN';
console.log(name.toLowerCase()); // wydrukuje: 'jan'
console.log(name); // wydrukuje: 'JAN'
```
Jak widać, oryginalny napis `name` pozostał niezmieniony.

## Zobacz także

Więcej informacji znajdziesz w dokumentacji JavaScript:

- [Metoda toLowerCase()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Metoda toLocaleLowerCase()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)