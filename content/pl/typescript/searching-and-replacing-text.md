---
title:                "Wyszukiwanie i zamienianie tekstu"
html_title:           "TypeScript: Wyszukiwanie i zamienianie tekstu"
simple_title:         "Wyszukiwanie i zamienianie tekstu"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Szukanie i zamiana tekstu to jedna z najczęstszych czynności wykonywanych przez programistów. Polega to na znalezieniu konkretnego ciągu znaków w tekście i jego zamianie na inny. Programiści często wykonują te czynności, aby szybko i łatwo zmieniać, poprawiać lub aktualizować kody źródłowe.

## Jak to zrobić:

```TypeScript
// Przykład 1:
let text = "Witaj świecie";
let newText = text.replace("Witaj", "Hello");
// Output: "Hello świecie"

// Przykład 2:
let text2 = "Lorem ipsum dolor sit amet";
let newText2 = text.replace("sit", "sitting");
// Output: "Lorem ipsum dolor sitting amet"
```

## Głębszy Przegląd

Wyszukiwanie i zamiana tekstu jest jedną z podstawowych operacji edycyjnych, z którą programiści mają do czynienia od początków programowania. Wcześniej była wykonywana ręcznie, ale dziś istnieje wiele narzędzi i funkcji w edytorach kodu, które ułatwiają ten proces. Alternatywą do szukania i zamiany jest użycie wyrażeń regularnych, które są bardziej zaawansowaną metodą manipulacji tekstem. W implementacji tekstowej, ważne jest uwzględnienie obecnej formy, wielkości liter i innych czynników.

## Zobacz również

- [Dokumentacja TypeScript - String Replace](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-1.html#template-literal-types)
- [Poradnik dla początkujących - Zamienianie tekstu w TypeScript](https://www.digitalocean.com/community/tutorials/typescript-zamienianie-tekstu)