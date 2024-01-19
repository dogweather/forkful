---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyszukiwanie i zamiana tekstu to technika manipulowania danymi za pomocą programowania. Programiści robią to do zautomatyzowania ciężkich lub monotonnych zadań związanych z tekstem, takich jak poprawa błędów ortograficznych w dużych dokumentach.

## Jak to zrobić:

Zobaczmy, jak zastąpić tekst w TypeScript. Użyjemy wbudowanej funkcji `replace()`.

```TypeScript
let tekst: string = "Witaj, świecie!";
let nowyTekst: string = tekst.replace("świecie", "TypeScript");
console.log(nowyTekst);
```

Wynik:

```
Witaj, TypeScript!
```

Metodę `replace()` używamy do wyszukiwania i zamiany części tekstu. Pierwszy argument to ciąg, który chcesz znaleźć. Drugi argument to ciąg, na który chcesz go zmienić.

Funkcję `replace()` możemy również używać z wyrażeniami regularnymi, aby zmieniać więcej niż jedno wystąpienie słowa.

```TypeScript
let tekst: string = "Kocham TypeScript. TypeScript jest wspaniały.";
let nowyTekst: string = tekst.replace(/TypeScript/g, "JavaScript");
console.log(nowyTekst);
```

Wynik:

```
Kocham JavaScript. JavaScript jest wspaniały.
```

## Deep Dive

Zastąpienie tekstu jest główną funkcją w większości języków programowania, a jego historia sięga dopóty, dopóki języki programowania początkowo obsługiwały dane tekstowe. W TypeScript, możemy użyć natywnej funkcji `replace()`, ale są również dostępne niezależne biblioteki, jak `lodash`, które oferują większą kontrolę i dodatkowe funkcje.

Można też użyć innych metod, takich jak `slice()` lub `substring()`, aby zastąpić tekst, ale `replace()` jest najprostszy i najbardziej zrozumiały.

## Zobacz też:

- Dokumentacja TypeScript: https://www.typescriptlang.org/docs/
- Dokumentacja lodash: https://lodash.com/docs/
- Wyrażenia regularne w JavaScript: https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Wyra%C5%BCenia_reguralne