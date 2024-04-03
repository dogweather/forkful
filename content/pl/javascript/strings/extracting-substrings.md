---
date: 2024-01-20 17:46:16.805106-07:00
description: "How to: JavaScript posiada kilka metod do wydobywania pod\u0142a\u0144\
  cuch\xF3w: `slice()`, `substring()`, i `substr()` (ale uwaga, `substr()` jest przestarza\u0142\
  a!). Oto\u2026"
lastmod: '2024-03-13T22:44:35.785503-06:00'
model: gpt-4-1106-preview
summary: "JavaScript posiada kilka metod do wydobywania pod\u0142a\u0144cuch\xF3w."
title: "Wycinanie pod\u0142a\u0144cuch\xF3w"
weight: 6
---

## How to:
JavaScript posiada kilka metod do wydobywania podłańcuchów: `slice()`, `substring()`, i `substr()` (ale uwaga, `substr()` jest przestarzała!). Oto jak działają:

```javascript
let text = "Zakopane jest super!";

// Użycie slice()
let sliced = text.slice(0, 8);
console.log(sliced); // "Zakopane"

// Użycie substring()
let substringed = text.substring(0, 8);
console.log(substringed); // "Zakopane"

// Użycie substr() - ale lepiej unikać
let substrd = text.substr(0, 8);
console.log(substrd); // "Zakopane"
```

## Deep Dive
Te metody mają swoje korzenie w językach, z których JavaScript czerpał inspiracje, jak Java czy C. Co ważne, `substr()` ma już etykietę przestarzałej i może zostać usunięta w przyszłych wersjach JavaScriptu, więc skłaniajmy się ku `slice()` czy `substring()`. `Slice()` i `substring()` różnią się tym, jak sobie radzą z ujemnymi indeksami (slice() je akceptuje, substring() zamienia ujemne indeksy na 0) i kilkoma innymi niuansami, które warto poznać po głębszym zanurzeniu w dokumentację.

```javascript
// slice() z ujemnymi indeksami
let negativeSlice = text.slice(-7, -1);
console.log(negativeSlice); // " jest s"

// substring() zamieni ujemne indeksy na 0
let negativeSubstring = text.substring(-7, 8); // To jest jak text.substring(0, 8);
console.log(negativeSubstring); // "Zakopane"
```

Czasami jednak chcemy użyć wyrażeń regularnych (regex), by wyłowić bardziej skomplikowane wzorce. Język JavaScript umożliwia takie manewry dzięki metodzie `match()`.

```javascript
let complexText = "ID: 12345, Data: 2023-01-30";
let regex = /ID: (\d+), Data: (\d{4}-\d{2}-\d{2})/;
let matches = complexText.match(regex);

console.log(matches[1]); // "12345" - ekstrakcja ID
console.log(matches[2]); // "2023-01-30" - ekstrakcja daty
```

## See Also:
1. MDN Web Docs na temat `slice()` - https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice
2. MDN Web Docs na temat `substring()` - https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring
3. MDN Web Docs na temat wyrażeń regularnych - https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
