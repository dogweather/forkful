---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

# TypeScript: Skriv Ut Debug Utdata

## Vad & Varför?

Att skriva ut debug-utdata är metoden vi använder för att visualisera värdet på variabler och tillståndet i applikationen under körning. Vi gör detta för att spåra och lösa fel (buggar) effektivt.

## Hur man gör:

Här är ett par exempel på hur man kan skriva ut "debug output" i TypeScript:

```TypeScript 
// Att använda 'console.log' är det mest grundläggande sättet
console.log("Hello, this is a debug message!");
```

```TypeScript
// Skriv ut värdet på variabel 'x'
let x = 5;
console.log('The value of x is: ', x);
// Output: The value of x is: 5
```

## Djupt Dyk:

### Historisk Kontext:
JavaScript och därmed TypeScript har alltid stödja `console.log` för utskrift av debug-utdata, en technik lånat från C-programmering.

### Alternativ:
Förutom `console.log`, kan du också använda `console.info`, `console.warn`, och `console.error` för att skriva ut meddelanden i olika nivåer av allvar.

```TypeScript 
console.info('This is an info message');
console.warn('This is a warning');
console.error('This is an error message');
```

### Implementeringsdetaljer:
`console.log` och dess syskonmetoder är inte en del av ECMAScript (JavaScript-specifikationen) utan tillhandahålls av värdmiljön (t.ex., webbläsare eller Node.js).

## Se Även:

För ännu mer information, kolla in följande artiklar:

- Mozilla Developer Network (MDN) om [`console`](https://developer.mozilla.org/sv-SE/docs/Web/API/Console) 
- Node.js docs om [`console`](http://nodejs.org/api/console.html)
- En mer genomgående guide till [JavaScript Debugging](https://javascript.info/debugging-chrome)