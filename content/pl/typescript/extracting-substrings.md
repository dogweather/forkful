---
title:                "TypeScript: Wycinanie podciągów"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Ekstrakcja podciągów jest często używanym i bardzo przydatnym narzędziem w programowaniu. Jest to proces wydobycia fragmentów tekstu lub ciągów znaków z łańcucha znaków. Czasami jest to konieczne do zastosowania w celu uzyskania odpowiednich danych lub do przeprowadzenia różnego rodzaju operacji na tekście.

## Jak to zrobić

Aby wykorzystać ekstrakcję podciągów w TypeScript, należy użyć metody `substring (beginIndex, endIndex)`, gdzie `beginIndex` jest indeksem pierwszego znaku w podciągu, a `endIndex`(opcjonalny) określa indeks ostatniego znaku w podciągu. Przykładowe użycie wyglądałoby następująco:

```TypeScript
let str: string = "Programowanie jest super";
let subStr: string = text.substring(6, 15);
console.log(subStr); // "owanie jest"
```

Można również użyć metody `slice (beginIndex, endIndex)`, która działa tak samo jak `substring`, ale `endIndex` jest opcjonalny i jeśli nie jest podany, wyodrębnia wszystkie znaki od `beginIndex` do końca łańcucha. Przykładowe użycie wyglądałoby następująco:

```TypeScript
let str: string = "Programowanie jest super";
let subStr: string = text.slice(6);
console.log(subStr); // "owanie jest super"
```

Możliwe jest również wykorzystanie funkcji `substr (beginIndex, length)`, która określa indeks pierwszego znaku w ekstrahowanym podciągu i jego długość. Przykładowe użycie wyglądałoby następująco:

```TypeScript
let str: string = "Programowanie jest super";
let subStr: string = text.substr(14, 5);
console.log(subStr); // "super"
```

## Głębszy zanurzenie

Podczas ekstrakcji podciągów należy pamiętać o kilku rzeczach. Po pierwsze, indeksy znaków zawsze zaczynają się od 0, więc pierwszy znak ma indeks 0, drugi ma indeks 1, itd. Po drugie, jeśli podamy indeks większy niż długość łańcucha, zostanie wyodrębniony pusty podciąg. Po trzecie, jeśli podamy niepoprawny indeks (np. wartość ujemna), zostanie zwrócony pusty łańcuch.

## Zobacz również

- [Dokumentacja dla metody substring](https://www.typescriptlang.org/docs/handbook/strings.html#substring)
- [Dokumentacja dla metody slice](https://www.typescriptlang.org/docs/handbook/strings.html#slice)
- [Dokumentacja dla funkcji substr](https://www.typescriptlang.org/docs/handbook/strings.html#substr)