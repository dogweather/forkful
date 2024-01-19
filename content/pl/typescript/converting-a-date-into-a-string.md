---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Zamieniamy Datę na Ciąg w TypeScript: Krótkie Wprowadzenie

## Dlaczego i po co?
Zamiana daty na ciąg znaków to proces, podczas którego obiekt daty jest konwertowany na ciąg znaków. Programiści robią to, aby łatwiej przechowywać, wyświetlać i manipulować datami w sposób dla ludzi zrozumiały.

## Jak to zrobić:
Podstawowym narzędziem jest metoda `toISOString()`. Sprawdź poniższy kod.
```TypeScript
let data: Date = new Date();
let ciag: string = data.toISOString();
console.log(ciag);
```
Po uruchomieniu kodu zobaczysz coś podobnego do tego: `2022-03-14T17:00:00.000Z`. To znaczy, że nasza data została poprawnie skonwertowana na ciąg znaków.

Jeśli chcesz to zrobić ręcznie, możesz użyć metod `getDay()`, `getMonth()`, `getFullYear()`, itp.

```TypeScript
let data: Date = new Date();
let ciag: string = `${data.getDate()}/${data.getMonth() + 1}/${data.getFullYear()}`;
console.log(ciag);
```

Wyjście będzie miało postać: `14/3/2022`.

## Pogłębione informacje
Metoda `toISOString()` jest częścią standardu ECMAScript od jego 5. edycji, opublikowanej w 2009 roku. Jest to sposób na konwersję daty do ciągu znaków zgodnie z formatem ISO 8601.

Alternatywą jest użycie bibliotek zewnętrznych, takich jak Moment.js, które oferują bardziej rozbudowane i konfigurowalne opcje konwersji dat.

W praktyce, metoda `toISOString()` polega na wywołaniu natywnych funkcji JavaScript do pobrania wartości dni, miesięcy i lat, a następnie sklejeniu ich razem w odpowiednim formacie.

## Zobacz też
1. [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
2. [ECMAScript Specifications](https://www.ecma-international.org/publications-and-standards/standards/ecma-262/)
3. [Moment.js library](https://momentjs.com/)
4. [ISO 8601 Date Formats](https://www.iso.org/iso-8601-date-and-time-format.html)