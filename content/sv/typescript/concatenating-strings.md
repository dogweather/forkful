---
title:                "Sammanfogning av strängar"
html_title:           "TypeScript: Sammanfogning av strängar"
simple_title:         "Sammanfogning av strängar"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konkatenera strängar innebär helt enkelt att man slår ihop flera strängar till en enda lång sträng. Programmerare gör detta för att lägga till dynamisk data till sina strängar, såsom användarens namn eller datum. Det är en effektiv och enkel metod för att skapa dynamiska texter inom programmering.

## Hur man gör:
```TypeScript
const name: string = "Sofie";
const welcomeMessage: string = "Välkommen till vår webbplats, " + name + "!";

console.log(welcomeMessage);
```
Output: Välkommen till vår webbplats, Sofie!

## Deep Dive:
Att konkatenera strängar är en mycket vanlig teknik inom programmering och används oftast för att bygga upp meddelanden och texter till användaren. Innan strängkonkatenering var möjligt, var det vanligt att använda komplexa metoder för att bygga upp dynamiska texter, vilket var mycket tidskrävande och gjorde koden svårare att läsa.

Alternativ till strängkonkatenering är att använda så kallade "template literals" som introducerades i ES6, vilket gör det möjligt att skriva strängar på ett mer effektivt sätt. Även om detta kan vara ett bättre alternativ för vissa programmerare, är strängkonkatenering fortfarande en mycket använd metod.

För att implementera strängkonkatenering i TypeScript använder man operatorn "+" för att slå ihop strängarna. Det är viktigt att notera att typerna på de olika strängarna måste matcha för att få ett korrekt resultat. Om man till exempel försöker konkatenera en sträng och ett nummer, kommer TypeScript att ge ett felmeddelande.

## Se även:
- [TypeScript - Strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [TypeScript - Template Literals](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)