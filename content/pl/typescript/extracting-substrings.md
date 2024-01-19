---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wyciąganie podciągów to proces wydzielania mniejszych segmentów, zwanych podciągami, z większego ciągu znaków. Programiści robią to, gdy potrzebują manipulować lub analizować mniejsze części danej strony.

## Jak to zrobić:

Używamy wbudowanej metody `substring()` w TypeScript do wyciągania podciągów. Poniżej znajduje się przykład:

```TypeScript
let str = "Cześć, jestem programistą!";
let subStr = str.substring(7, 13);
console.log(subStr);  // "jestem"
```

W tym przykładzie `substring(7, 13)` wyciąga podciąg zaczynający się od 7. miejsca (licząc od 0), a kończący na 13. miejscu. 

Użytkowanie `substring()` jest proste. Pamiętaj jednak, że TypeScript (podobnie jak JavaScript) liczy miejsca od 0, co oznacza, że pierwszy znak w ciągu to miejsce 0, a nie 1.

## Deep Dive

Metoda `substring()` jest długoletnią częścią JavaScript, na którym TypeScript jest oparty. Jest często używana ze względu na swoją prostotę i efektywność, choć nie jest jedyną metodą do manipulacji ciągami w TypeScript. Alternatywą jest `slice()`, który działa podobnie, ale ma nieco większą elastyczność, np. umożliwia użycie wartości ujemnych.

Pod względem implementacji, `substring()` działa przez przeiterowanie przez ciąg znaków i zwracanie nowego ciągu zawierającego znaki między określonymi indeksami.

## Zobacz też:

- Dokumentacja TypeScript na temat ciągów: [https://www.typescriptlang.org/docs/handbook/strings.html](https://www.typescriptlang.org/docs/handbook/strings.html)
- Porównanie `substring()` i `slice()`: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- Szczegółowy przewodnik po ciągach w TypeScript: [https://www.digitalocean.com/community/tutorials/typescript-strings](https://www.digitalocean.com/community/tutorials/typescript-strings)