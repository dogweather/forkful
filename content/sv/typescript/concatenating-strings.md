---
title:                "Sammanslagning av strängar"
html_title:           "TypeScript: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför 
Det finns många tillfällen då man behöver slå samman flera textsträngar i en programkod. Det kan vara för att skapa en fullständig URL, skapa ett meddelande eller kombinera variabler med text. Oavsett syftet, så är det viktigt att veta hur man slår ihop strängar på ett korrekt sätt för att undvika fel i koden.

## Hur man gör
Om man använder TypeScript för att utveckla applikationer, så finns det flera olika sätt att konkatenera strängar. Det enklaste sättet är att använda + -operatören för att slå ihop två strängar. Till exempel:

```TypeScript
let förnamn: string = "Anna";
let efternamn: string = "Andersson";
let fullständigtNamn: string = förnamn + " " + efternamn;

console.log(fullständigtNamn); // Output: Anna Andersson
```

Man kan också använda en string template för att slå ihop strängar. String templates är särskilt användbara när man vill inkludera variabler i en sträng. Till exempel:

```TypeScript
let pris: number = 150;
let produkt: string = "Sko";

let meddelande: string = `Produkten ${produkt} kostar ${pris} kr.`;

console.log(meddelande); // Output: Produkten Sko kostar 150 kr.
```

Det är också möjligt att använda String.concat() -metoden för att slå ihop flera strängar samtidigt. Detta är särskilt användbart om man behöver slå ihop en större mängd strängar. Till exempel:

```TypeScript
let förnamn: string = "Anna";
let efternamn: string = "Andersson";
let yrke: string = "utvecklare";

let fullständigtNamn: string = String.concat(förnamn, " ", efternamn, " är en ", yrke);

console.log(fullständigtNamn); // Output: Anna Andersson är en utvecklare
```

## Djupdykning
När man använder + -operatören för att konkatenera strängar i TypeScript, så bör man vara medveten om att om man försöker konkatenera en sträng med en annan typ, så kommer den andra typen att konverteras till en sträng. Detta kan orsaka problem om man inte är medveten om det. Till exempel:

```TypeScript
let tal: number = 5;
let text: string = "10";

let resultat: string = tal + text;

console.log(resultat); // Output: 510
```

Som du kan se, så konverterades talet 5 automatiskt till en sträng och sedan konkatenerades den med strängen "10", vilket resulterade i strängen "510". Detta kan leda till oönskade resultat om man inte är medveten om konverteringen.

Det är också viktigt att notera att man inte bör använda `+=` -operatören för att konkatenera strängar i TypeScript. Detta kan ge oönskade resultat eftersom `+=` -operatören även används för att tilldela ett värde till en variabel. Till exempel:

```TypeScript
let text: string = "Hej";
text += "välkommen!";

console.log(text); // Output: Hejvälkommen!
```

Som du kan se i exemplet ovan, så konkatenerades "välkommen!" till variabeln text, men det är inte det resultatet som man kanske hade förväntat sig om man enbart ville slå ihop strängar. Det är därför bättre att använda + -operatören för konkatenering istället för `+=`.

## Se även
- [TypeScript Playground](https://www.typescriptlang.org/play/)
- [Official TypeScript Documentation](https://www.typescriptlang.org/docs/)