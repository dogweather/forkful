---
date: 2024-01-20 17:59:11.451435-07:00
description: "Wyszukiwanie i zamiana tekstu to podstawa przetwarzania ci\u0105g\xF3\
  w znak\xF3w, umo\u017Cliwiaj\u0105ca podmian\u0119 okre\u015Blonych fragment\xF3\
  w na nowe. Programi\u015Bci korzystaj\u0105 z\u2026"
lastmod: '2024-03-13T22:44:35.123042-06:00'
model: gpt-4-1106-preview
summary: "Wyszukiwanie i zamiana tekstu to podstawa przetwarzania ci\u0105g\xF3w znak\xF3\
  w, umo\u017Cliwiaj\u0105ca podmian\u0119 okre\u015Blonych fragment\xF3w na nowe."
title: Wyszukiwanie i zamiana tekstu
weight: 10
---

## What & Why? (Co i dlaczego?)
Wyszukiwanie i zamiana tekstu to podstawa przetwarzania ciągów znaków, umożliwiająca podmianę określonych fragmentów na nowe. Programiści korzystają z tych operacji, by modyfikować dane, automatyzować refaktoryzację kodu i usprawniać pracę z tekstami.

## How to: (Jak to zrobić?)
```TypeScript
// Podstawowe wyszukiwanie i zamiana tekstu
let text: string = "Cześć, tu TypeScript!";
let replacedText: string = text.replace("TypeScript", "świat");
console.log(replacedText); // Output: Cześć, tu świat!

// Użycie wyrażeń regularnych do zamiany wszystkich wystąpień
let regexText: string = "Jabłka są fajne. Jabłka są zdrowe.";
let regexReplacedText: string = regexText.replace(/Jabłka/gi, "Banany");
console.log(regexReplacedText); // Output: Banany są fajne. Banany są zdrowe.

// Funkcja zamiany z użyciem callback
let complexText: string = "Programowanie w TypeScript to zabawa.";
let modifiedText: string = complexText.replace(/TypeScript/g, (match) => {
    return match.toUpperCase();
});
console.log(modifiedText); // Output: Programowanie w TYPESCRIPT to zabawa.
```

## Deep Dive (Pogłębione spojrzenie)
Wyszukiwanie i zamiana tekstu opiera się na mechanizmach wyrażeń regularnych, które sięgają lat 50. XX wieku. Na przestrzeni lat rozwinęły się różne dialekty i warianty, takie jak Perl Compatible Regular Expressions (PCRE), na których częściowo bazuje JavaScript i TypeScript.

Alternatywy dla wyrażeń regularnych to metody jak `indexOf` czy `includes`, które pomagają w lokalizowaniu tekstu, ale bez opcji zamiany. Do bardziej skomplikowanych przypadków przydają się biblioteki jak lodash, oferujące zaawansowane funkcje przetwarzania ciągów znaków.

Ważne w implementacjach szukania i zamiany tekstu jest również zarządzanie wydajnością przy długich ciągach i częstych operacjach, co może być krytyczne w aplikacjach o dużym obciążeniu.

## See Also (Zobacz również)
- [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace) - Dokumentacja metody `replace` w JavaScript, na której opiera się TypeScript.
- [RegExp - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions) - Przewodnik po wyrażeniach regularnych w JS.
- [lodash - GitHub](https://github.com/lodash/lodash) - Repozytorium biblioteki lodash, narzędzia do pracy z ciągami znaków.
