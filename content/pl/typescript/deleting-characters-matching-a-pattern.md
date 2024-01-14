---
title:                "TypeScript: Usuwanie znaków pasujących do wzorca"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Kasowanie znaków pasujących do wzoru jest częstą potrzebą w programowaniu. Możemy chcieć pozbyć się zbędnych spacji, przecinków czy też innych znaków specjalnych. Dzięki usuwaniu niepotrzebnych znaków, nasz kod może stać się czytelniejszy i łatwiejszy do zrozumienia.

## Jak to zrobić

Aby usunąć znaki pasujące do wzoru w TypeScript, możemy użyć wbudowanej metody `replace()` w połączeniu z wyrażeniem regularnym. Poniżej przedstawione są dwa przykładowe sposoby:

```TypeScript
// Przykład 1: Usuwanie spacji z tekstu

let tekst = "Hello World!";
let nowyTekst = tekst.replace(/\s/g, ""); // wyrażenie regularne znajdujące spację i zamieniające ją na pusty ciąg znaków
console.log(nowyTekst); // w konsoli wyświetli się "HelloWorld!"

// Przykład 2: Usuwanie znaków specjalnych z tekstu

let tekst = "He$$o, W#rld!";
let nowyTekst = tekst.replace(/[^\w\s]/gi, ""); // wyrażenie regularne znajdujące wszystkie znaki specjalne i usuwające je
console.log(nowyTekst); // w konsoli wyświetli się "Hello World"
```

## Dogłębny przebieg

Wyrażenia regularne są narzędziem wykorzystywanym do manipulacji tekstu. Pozwalają one określić wzorzec, który chcemy znaleźć lub zastąpić w tekście. W przypadku usuwania znaków, możemy użyć znaku specjalnego `\s` w celu znalezienia spacji lub `\w` w celu znalezienia znaków alfanumerycznych.

Wymienione przykłady nie uwzględniają wszystkich możliwości zastosowań wyrażeń regularnych. Bardziej skomplikowane wzorce mogą wymagać większej wiedzy na temat ich składni, jednakże są one bardzo wartościowym narzędziem w programowaniu.

## Zobacz także

- [Dokumentacja TypeScript: Metoda replace()](https://www.typescriptlang.org/docs/handbook/strings.html#replace)
- [W3Schools: Wzorce wyrażeń regularnych](https://www.w3schools.com/js/js_regexp.asp)
- [Regex101: Tests for Regex DNA Worksheet](https://regex101.com/test/dna) (przydatne narzędzie do testowania wyrażeń regularnych)