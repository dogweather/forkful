---
title:                "Hitta längden på en sträng"
html_title:           "TypeScript: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng kan vara en vanligt förekommande uppgift vid programmering. Det kan vara för att jämföra olika strängar eller för att validera inmatade användardata. Genom att kunna hantera stränglängder effektivt kan man skriva mer robust och effektiv kod.

## Hur man gör det

Att hitta längden på en sträng i TypeScript är enkelt. Det finns ett inbyggt tillvägagångssätt för att räkna antalet tecken i en sträng, som vi kommer att utforska nedan.

Först bör vi tilldela en sträng till en variabel:

```TypeScript
let sträng = "Hej allihopa!"
```

För att hitta längden på denna sträng kan vi använda .length-egenskapen:

```TypeScript
console.log(sträng.length) // Outputs: 14
```

Här har vi använt console.log() för att skriva ut längden på vår sträng i konsolen. Notera att det är .length och inte .Length, eftersom det är ett egenskapsattribut, inte en metod.

## Djupdykning

Det är viktigt att förstå att length-egenskapen räknar antalet tecken i en sträng, inte antalet ord. Till exempel:

```TypeScript
let sträng = "Denna sträng innehåller fem ord."
console.log(sträng.length) // Outputs: 36
```

Observera att mellanslag räknas som ett tecken, så det totala antalet tecken i denna sträng är 36, trots att det finns endast fem ord.

Det finns också funktioner som .trim() som kan hjälpa till att räkna antalet tecken i en sträng utan att inkludera mellanslag i början eller slutet av strängen.

Att använda .length-egenskapen på en tom sträng returnerar 0.

## Se även

Här är några artiklar för vidare läsning om strängar och TypeScript:

- [Officiell TypeScript-dokumentation om strängar](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [En guide till strängmanipulering i TypeScript](https://www.smashingmagazine.com/2019/02/string-manipulation-series-typrescript/)
- [10 användbara TypeScript-stringfunktioner och -egenskaper](https://www.javascripttutorial.net/typescript/typescript-string/)