---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Usuwanie znaków zgodnych z określonym wzorcem to proces wykorzystywany do czyszczenia danych wejściowych, tego typu operacje są często stosowane podczas przetwarzania literałów łańcuchowych. Programiści stosują to narzędzie, aby kontrolować i ulepszać jakość danych.

## Jak to zrobić:

Oto kilka przykładów pokazujących, jak możemy usunąć wzorzec znaków w TypeScript. Wykorzystajmy wbudowaną metodę `replace()`, która pozwala nam zastąpić określony fragment ciągu innym.

```TypeScript
let str = "Cześć, jestem programistą TypeScript!";
  
// Usuńmy wszystkie wystąpienia litery 'a'
let newStr = str.replace(/a/g, '');
console.log(newStr);  // "Cześć, jestem progrmistą TypeScript!"
```

## Deep Dive

(1) Historyczny kontekst: Metoda 'replace' jest częścią oryginalnego zestawu narzędzi JavaScript, który został wprowadzony w ES5. Odkąd TypeScript jest nadzbiorem JavaScript, ta metoda jest również dostępna.
  
(2) Alternatywy: Możemy również użyć metody `split().join()` aby usunąć określone znaki ze stringów.  

```TypeScript
let str = "Cześć, jestem programistą TypeScript!";
let newStr = str.split('a').join('');
console.log(newStr); // "Cześć, jestem progrmistą TypeScript!"
```

(3) Szczegóły implementacji: Metoda `replace()` używa wyrażeń regularnych do identyfikacji wzorca, który ma zostać zastąpiony. Warunkiem koniecznym jest dodanie flagi 'g' do wyrażenia, aby zastąpić wszystkie wystąpienia, a nie tylko pierwsze.
  
## Zobacz także

- Dokumentacja MDN na temat metody `replace()`: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Dokumentacja MDN na temat wyrażeń regularnych: https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions
- Wprowadzenie do TypeScript (Oficjalna strona TypeScript): https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html