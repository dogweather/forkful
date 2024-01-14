---
title:                "TypeScript: I säkning av längden på en sträng"
simple_title:         "I säkning av längden på en sträng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför?

Det finns många tillfällen där man behöver hitta längden av en sträng i TypeScript. Det kan vara för att göra kontroller på inmatade data eller för att manipulera strängar i ett program. Att kunna hitta längden på en sträng är en grundläggande funktion som är användbar i många olika sammanhang.

## Hur man gör det?

För att hitta längden på en sträng i TypeScript använder man sig av .length-metoden. Detta förutsätter att man redan har en sträng att arbeta med. Nedan följer ett exempel på hur man kan skriva en funktion som tar en inmatad sträng och returnerar dess längd:

```TypeScript
function hittaLängd(sträng: string): number {
    return sträng.length;
}

// Exempel på hur funktionen kan anropas:

let mittNamn: string = "Johanna";
let mittNamnsLängd: number = hittaLängd(mittNamn);

console.log("Längden på mitt namn är: " + mittNamnsLängd);

// Output: Längden på mitt namn är: 7
```

Som ni kan se i exemplet ovan så behöver man bara använda sig av strängens .length-metod för att få fram dess längd. Denna metod fungerar på alla strängar oavsett om de består av bokstäver, siffror eller specialtecken.

## Utforska vidare

Nu när ni vet hur man enkelt kan hitta längden på en sträng i TypeScript så är det dags att utforska vidare. Det finns flera funktioner och metoder som kan användas tillsammans med .length för att manipulera och utforska strängar. Nedan följer några länkar som kan vara användbara för er som vill lära er mer:

- [Dokumentation om strängar i TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [W3Schools - String Methods](https://www.w3schools.com/js/js_string_methods.asp)
- [TypeScript String Functions Tutorial](https://appdividend.com/2019/03/29/typescript-string-functions-tutorial-with-examples/)

## Se även

- [Mer om TypeScript på dev.to](https://dev.to/t/typescript)
- [En guide till TypeScript för nybörjare](https://medium.com/swlh/a-beginners-guide-to-typescript-4118a612598e)
- [Github repository för TypeScript](https://github.com/microsoft/TypeScript)