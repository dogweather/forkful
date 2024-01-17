---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "Javascript: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Usuwanie znaków pasujących do wzorca to czynność, która polega na wyeliminowaniu wszystkich znaków ze stringa, które odpowiadają określonemu wzorcowi. Programiści często wykonują tę czynność w celu oczyszczenia danych lub manipulacji tekstem.

## Jak to zrobić:

Przykłady kodu i wyników:

```javascript
const string = "Hello, World!";
console.log(string.replace(/[aeiou]/g, '')); // Hll, Wrld!
```

```javascript
const numbers = [-10, 20, 30, -40, 50];
console.log(numbers.filter(num => num > 0)); // [20, 30, 50]
```

## Głębsza analiza:

1. Kontekst historyczny: Usuwanie znaków pasujących do wzorca jest jednym z podstawowych zagadnień związanych z manipulacją tekstem w programowaniu. Zostało wprowadzone w językach programowania już w latach 60.
2. Alternatywy: Oprócz wykorzystania metody `replace` można również użyć metody `filter`, jak pokazano w drugim przykładzie powyżej. Niektórzy programiści mogą również użyć pętli `for` lub `forEach`.
3. Szczegóły implementacji: W Javascript możemy wykorzystać wyrażenia regularne (RegExp) do precyzyjnego wskazania wzorca znaków, które chcemy usunąć.

## Zobacz także:

- [Dokumentacja JS o metodzie replace](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Dokumentacja JS o metodzie filter](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/filter)
- [Artykuł na temat manipulacji tekstem w JS](https://www.digitalocean.com/community/tutorials/how-to-manipulate-strings-in-javascript)