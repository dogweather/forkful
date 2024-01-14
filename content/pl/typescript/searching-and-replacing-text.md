---
title:    "TypeScript: Wyszukiwanie i zamiana tekstu"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Nie ma wątpliwości, że każdy programista często musi korzystać z funkcji wyszukiwania i zamiany tekstu w swoim kodzie. To nie tylko ułatwia pracę, ale także może poprawić czytelność i wydajność kodu. W tym artykule dowiesz się dlaczego warto używać tej funkcji i jak można ją wykorzystać w języku TypeScript.

## Jak to zrobić

Aby wyszukać i zamienić tekst w kodzie TypeScript, należy użyć funkcji `replace()`. Przykładowy kod poniżej pokazuje jak można zastosować tę funkcję:

```TypeScript
let text = "Hello World";
text = text.replace("World", "Polish Readers");
console.log(text);
```

Wyjściem z tego kodu będzie `"Hello Polish Readers"`, ponieważ funkcja `replace()` zamieniła słowo "World" na "Polish Readers". Można również wykorzystać regularne wyrażenia w celu bardziej złożonych wyszukiwań i zamian.

## Deep Dive

Funkcja `replace()` może przyjmować różne parametry, co pozwala na bardziej zaawansowane operacje wyszukiwania i zamiany tekstu. Można na przykład dodać flagi, które określają sposób wyszukiwania tekstu (np. z uwzględnieniem wielkości liter) lub zastosować funkcję zwrotną do dostosowania zamian. Istnieje również możliwość przekazania wyrażenia regularnego jako parametru wyszukiwania, co daje jeszcze większą kontrolę nad operacją zamiany.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o funkcji `replace()` w języku TypeScript, zapoznaj się z dokumentacją na stronie [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/basic-types.html). Możesz również przeczytać artykuł [Regular Expressions in TypeScript](https://blog.logrocket.com/regular-expressions-in-typescript/), który pokaże Ci jak wykorzystać wyrażenia regularne w tej funkcji.