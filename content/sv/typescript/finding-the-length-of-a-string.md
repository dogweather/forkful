---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad och varför?

Att hitta längden på en sträng i programmering innebär att avgöra antalet tecken i strängen. Programmerare gör detta för att manipulera information mer effektivt, till exempel vid iterering genom strängar eller validering av användarinput.

## Hur man gör:
Det är mycket enkelt att få längden på en sträng i TypeScript. Använd `.length` egenskapen. Här är ett exempel:

```typescript
let str: string = 'Hej, världen!';
console.log(str.length); // Output: 14
```

## Djupdykning

Att hitta längden på en sträng har sina rötter i tidiga programmeringsspråk, och är fundamentalt för textmanipulering.
 
Alternativen till `.length` kan variera beroende på vilket språk du använder. Till exempel, i vissa språk kan du behöva importera ett bibliotek eller skriva en egen funktion för att få stränglängden.

När det gäller `.length` i TypeScript, implementeras det faktiskt på samma sätt som i JavaScript. När en sträng skapas i minnet, skapas även en `.length` egenskap som håller koll på antalet tecken i strängen.

## Se också

För mer information om arbetet med strängar i TypeScript, besök följande länkar: 

- [String - TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [JavaScript String Length Property (kan tillämpas på TypeScript)](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [TypeScript - String length Property - Tutorialsteacher](https://www.tutorialsteacher.com/typescript/typescript-string-length)