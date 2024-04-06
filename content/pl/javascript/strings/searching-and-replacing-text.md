---
date: 2024-01-20 17:57:57.891942-07:00
description: "How to (Jak to zrobi\u0107) Proste, prawda? U\u017Cywamy `.replace()`\
  \ z wyra\u017Ceniem regularnym `/blue/g` by zamieni\u0107 wszystkie przyk\u0142\
  ady \"blue\" na \"red\"."
lastmod: '2024-04-05T22:37:44.535235-06:00'
model: gpt-4-1106-preview
summary: "How to (Jak to zrobi\u0107) Proste, prawda? U\u017Cywamy `.replace()` z\
  \ wyra\u017Ceniem regularnym `/blue/g` by zamieni\u0107 wszystkie przyk\u0142ady\
  \ \"blue\" na \"red\"."
title: Wyszukiwanie i zamiana tekstu
weight: 10
---

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
