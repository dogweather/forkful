---
title:                "Interpolacja ciągu znaków"
html_title:           "Javascript: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O co chodzi i dlaczego?

Interpolowanie stringów to proces łączenia różnych wartości, takich jak zmienne czy stałe, z ustalonym tekstem w celu utworzenia jednego, spójnego stringa. Programiści używają tego narzędzia, aby ułatwić sobie pracę z wieloma wartościami i uniknąć konieczności pisania długich i skomplikowanych ciągów znaków.

## Jak to zrobić:

```javascript
const name = "Kasia";
const age = 29;

// using template literals
const message = `Cześć, nazywam się ${name} i mam ${age} lat!`;
console.log(message);

// output: Cześć, nazywam się Kasia i mam 29 lat!
```

```javascript
const fruit = "jabłko";
const price = 2.50;

// using string concatenation
const message = "Cena za " + fruit + " to " + price + " zł.";
console.log(message);

// output: Cena za jabłko to 2.50 zł.
```

## Głębsza analiza:

Interpolowanie stringów stało się możliwe dzięki wprowadzeniu w 2015 roku tzw. "template literals" w specyfikacji ECMAScript 6. Alternatywą dla tej metody jest łączenie stringów za pomocą operatora "+" lub wykorzystanie funkcji "String.raw()". W implementacji interpolacji warto zwrócić uwagę na sposób działania składników zmiennych, takich jak wartości "undefined", "null" lub obiektów, które zostaną automatycznie przekonwertowane na ich stringowe reprezentacje.

## Zobacz też:

- Dokumentacja na temat interpolowania stringów: https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Template_literals
- Porównanie różnych metod tworzenia stringów: https://davidwalsh.name/template-strings-javascript
- Krótki tutorial na temat ES6 template literals:
https://www.javascripttutorial.net/es6/es6-template-literals/