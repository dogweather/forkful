---
title:                "Interpolering av en sträng"
html_title:           "Arduino: Interpolering av en sträng"
simple_title:         "Interpolering av en sträng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Arbeta med stränginterpolation i TypeScript

## Vad & Varför?
Stränginterpolation är en process för att infoga eller kommer in värden inuti en stränglitteral. Den används för att göra kod mer läsbar och underlätta formatet för dynamiska strängar.

## Hur man:
Här är ett exempel på hur man använder stränginterpolation i TypeScript:

```TypeScript
let name = "Kalle";
let age = 28;

let greeting = `Hej ${name}, du är ${age} år gammal.`;

console.log(greeting);  // Output:  Hej Kalle, du är 28 år gammal.
```
I detta fall kan variablerna `name` och `age` variera, vilket gör stränginterpolation mycket användbar för att skapa dynamiska strängar.

## Djupdykning
1. Historisk kontext: Stränginterpolation har existerat i programmering i många år, och har använts i många programmeringsspråk inklusive Ruby och Python innan TypeScript lade till det. 
2. Alternativ: Innan stränginterpolation fanns, använde utvecklare konkatenering, som kan bli rörigt och svårläst när du arbetar med flera variabler.
3. Implementeringsinformation: I TypeScript skrivs stränginterpolation med hjälp av bakåtstreck (\`) för att definiera strängarna och `${}` för att inkapsla de variabler eller uttryck som ska interpoleras.

## Se också
Om du vill lära dig mer om stränginterpolation i TypeScript och dess användning, se följande resurser:
2. Stack Overflow: [Hur man använder stränginterpolation i TypeScript](https://stackoverflow.com/questions/3304014/how-to-interpolate-strings-in-typescript)

Kom ihåg att stränginterpolation kan gör din kod mer läsbar och underlätta skapandet av dynamiska strängar!