---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konvertera en sträng till gemen (lower case) innebär att ändra alla stora bokstäver i en sträng till små. Programmerare gör detta vanligtvis för att standardisera datainsamling och underlätta jämförelser eller sökningar, då JavaScript är skiftlägeskänslig.

## Hur till:

I TypeScript, kan du använda metoden `toLowerCase()` för att konvertera alla tecken i en sträng till gemener. 

```TypeScript
let minStrang: string = "HeJ SvErIgE!";
console.log(minStrang.toLowerCase()); // utskrift: "hej sverige!"
```

I kodbiten ovan skapas en sträng `minStrang`. Vi använder sedan metoden `toLowerCase()` för att konvertera strängen till små bokstäver och skriva ut den.

## Fördjupning

Funktionen att konvertera strängar till gemener har funnits i programmeringsspråk sedan länge, särskilt eftersom de är skiftlägeskänsliga. TypeScript, som är ett superset av JavaScript, har ärvt denna funktion.

Ett alternativ till `toLowerCase()` är `toLocaleLowerCase()`. Skillnaden ligger i att `toLocaleLowerCase()` tar hänsyn till språkspecifika regler för konvertering av strängar till små bokstäver. I de flesta fall kommer dock resultaten vara desamma.

```TypeScript
let minStrang: string = "HeJ ÅÄÖ!";
console.log(minStrang.toLocaleLowerCase('sv-SE')); // utskrift: "hej åäö!"
```

Observera att `toLowerCase()` - och också `toLocaleLowerCase()` - inte ändrar den ursprungliga strängen. De returnerar en ny sträng.

## Se även:

1. [MDN Web Docs: toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
2. [MDN Web Docs: toLocaleLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
3. [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/basic-types.html#string) 
4. [StackOverflow: Difference between toLowerCase() and toLocaleLowerCase()](https://stackoverflow.com/questions/26882631/javascript-difference-between-tolowercase-and-tolocalelowercase)