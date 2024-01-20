---
title:                "Łączenie ciągów znaków"
html_title:           "Arduino: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

# Kolokwialnie o konkatenacji łańcuchów znakowych w TypeScript

## Co to jest i dlaczego?

Konkatenacja łańcuchów znakowych polega na łączeniu dwóch lub więcej ciągów znakowych w jeden dłuższy. Programiści robią to, aby tworzyć bardziej złożone wiadomości lub instrukcje bez konieczności utrzymywania wielu oddzielnych ciągów.

## Jak to zrobić:

W TypeScript możesz połączyć stringi na kilka sposobów. Oto najprostsze z nich:

```TypeScript
let str1 = 'Cześć';
let str2 = ' świecie!';
let message = str1 + str2;
console.log(message); // Wydrukuj: 'Cześć świecie!'
```

Możesz także używać szablonów literałów ciągu znaków do konkatenacji, co pozwala na bardziej zwięzłą składnię:

```TypeScript
let str1 = 'Cześć';
let str2 = 'świecie';
let message = `${str1}, ${str2}!`;
console.log(message); // Wydrukuj: 'Cześć, świecie!'
```

## Deep Dive

Konkatenacja łańcuchów znakowych to stara i prosta technika, ale sprawdza się w wielu sytuacjach. W TypeScript działa bardzo podobnie jak w większości języków programowania.

Alternatywą dla połączenia stringów może być użycie metod `concat()` lub `join()`. Metoda `concat()` scala dwa łańcuchy, a `join()` łączy elementy tablicy w jeden string.

Szczegóły implementacji w TypeScript są dość proste: operatory `+` i `` ` `` są używane do łączenia łańcuchów znakowych. Operator `+` po prostu łączy łańcuchy razem, podczas gdy szablony literałów ciągu znaków pozwalają na wrzucanie zmiennych i wyrażeń bezpośrednio w tekst.

## Zobacz również:

Jeśli chcesz dowiedzieć się więcej o konkatenacji łańcuchów znakowych, oto kilka użytecznych źródeł:

- Dokumentacja na temat stringów w TypeScript: [https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string)
- Szczegóły na temat szablonów literałów template string: [https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)