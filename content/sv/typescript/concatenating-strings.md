---
title:                "TypeScript: Sammanslagningssträngar"
simple_title:         "Sammanslagningssträngar"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför
Att sammanbinda eller konkatenera strängar är en vanlig uppgift inom programmering. Genom att kombinera flera enskilda strängar till en enda kan du skapa mer dynamiska och meningsfulla meddelanden och texter.

## Hur man gör
För att sammanbinda strängar i TypeScript kan du använda operatorn "+" eller metoden ".concat()". Nedan följer två exempel på hur man skulle kunna sammanbinda två strängar "Hej" och "världen" för att skapa texten "Hej världen".

```
TypeScript

let string1: string = "Hej";
let string2: string = "världen";

let result1: string = string1 + " " + string2;

let result2: string = string1.concat(" ", string2);

console.log(result1); // "Hej världen"
console.log(result2); // "Hej världen"
```

Det finns också möjlighet att använda template literals med hjälp av backticks (`) för att enklare kunna sammanbinda flera strängar. Nedan följer ett exempel på hur dessa kan användas för att skapa samma resultat som ovan.

```
TypeScript

let string1: string = "Hej";
let string2: string = "världen";

let result: string = `${string1} ${string2}`;

console.log(result); // "Hej världen"
```

## Djupdykning
När vi sammanbinder strängar i TypeScript är det viktigt att tänka på exakt vilken typ av data vi hanterar. Om vi till exempel försöker sammanbinda en sträng med ett numeriskt värde kan det leda till oönskade resultat. I sådana fall bör vi se till att konvertera det numeriska värdet till en sträng för att undvika problem.

En annan viktig aspekt är prestanda. Att sammanbinda strängar kan vara en relativt resurskrävande operation, särskilt om man använder operatorn "+" som kan resultera i en del onödig minnesallokering. I dessa fall kan det vara effektivare att använda metoden ".concat()" som inte skapar nya strängar utan istället modifierar den befintliga.

## Se även
- [TypeScript: Basic Types](https://www.typescriptlang.org/docs/handbook/basic-types.html)
- [MDN: String Concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Addition_assignment)
- [Stack Overflow: String Concatenation in TypeScript](https://stackoverflow.com/questions/19255780/typescript-string-concatenation/19256591)