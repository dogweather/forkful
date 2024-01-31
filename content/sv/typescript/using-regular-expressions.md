---
title:                "Använda reguljära uttryck"
date:                  2024-01-19
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Regular expressions, eller regex, är mönster som används för att matcha karaktärsföljder i text. Programmerare använder det för att söka, validera, extrahera och manipulera data effektivt.

## Hur gör man:
```TypeScript
const text = "Hitta nummer: 073-1234567";
const regexPattern = /\d{3}-\d{7}/;
const match = text.match(regexPattern);

console.log(match[0]); // Skriver ut "073-1234567"
```

```TypeScript
const email = "exempel@domain.se";
const emailPattern = /\S+@\S+\.\S+/;
const isValidEmail = emailPattern.test(email);

console.log(isValidEmail); // Skriver ut true
```

## Fördjupning
Regular expressions har använts sedan 1950-talet; de integrerades först i Unix-verktyg som `grep`. Alternativ till regex inkluderar string-metoder som `indexOf()` eller `includes()`, men dessa är oftast mindre kraftfulla. I TypeScript körs regex via JavaScript engine (som V8 i Chrome), och dess prestanda kan variera mellan olika webbläsare och Node.js.

## Se även
- MDN Regular Expressions Guide:
  [developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- RegExp i TypeScript Handbook:
  [typescriptlang.org/docs/handbook/2/everyday-types.html#regex](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#regexp)
- Regex101 (för att testa regex online):
  [regex101.com](https://regex101.com)
