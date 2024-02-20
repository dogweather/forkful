---
date: 2024-01-20 17:57:57.891942-07:00
description: "Szukanie i zamiana tekstu to zmiana fragment\xF3w tekstu na inne. Programi\u015B\
  ci robi\u0105 to dla automatyzacji, edycji danych i naprawy b\u0142\u0119d\xF3w."
lastmod: 2024-02-19 22:04:54.931137
model: gpt-4-1106-preview
summary: "Szukanie i zamiana tekstu to zmiana fragment\xF3w tekstu na inne. Programi\u015B\
  ci robi\u0105 to dla automatyzacji, edycji danych i naprawy b\u0142\u0119d\xF3w."
title: Wyszukiwanie i zamiana tekstu
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Szukanie i zamiana tekstu to zmiana fragmentów tekstu na inne. Programiści robią to dla automatyzacji, edycji danych i naprawy błędów.

## How to (Jak to zrobić)
```Javascript
let text = "Mr Blue has a blue house and a blue car";
let newText = text.replace(/blue/g, "red");

console.log(newText); // Mr Blue has a red house and a red car
```
Proste, prawda? Używamy `.replace()` z wyrażeniem regularnym `/blue/g` by zamienić wszystkie przykłady "blue" na "red".

## Deep Dive (Głębsze zagłębienie)
Historia: Wyrażenia regularne (regex) mają militarne korzenie, opracowane w latach 50. Alternatywy: Jeśli regex jest overkill, są metody jak `.split()` i `.join()`. Implementacja: JavaScript `.replace()` używa pierwszego argumentu jako wzorca do wyszukiwania, który może być stringiem lub regexem, i drugiego jako stringa zastępującego.

## See Also (Zobacz również)
- MDN replace documentation: [developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- Regex guide: [regular-expressions.info](https://www.regular-expressions.info/)
- More on split and join methods: [w3schools.com](https://www.w3schools.com/jsref/jsref_obj_string.asp)
