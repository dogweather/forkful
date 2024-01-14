---
title:    "TypeScript: Sammanslående strängar"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanfoga strängar är en vanlig och användbar teknik inom programmering. Genom att kombinera flera strängar kan man skapa mer dynamiska och anpassningsbara texter, vilket är särskilt viktigt i webbutveckling. Det kan också hjälpa till att effektivisera koden och göra den mer läsbar för andra utvecklare.

## Så här gör du

För att sammanfoga strängar i TypeScript kan man använda operatorn "+" eller metoden "concat()". Här är några exempel:

```TypeScript
let förnamn: string = "Anna";
let efternamn: string = "Andersson";
// Använda operatorn "+" 
console.log(förnamn + " " + efternamn); // Output: "Anna Andersson"
// Använda metoden concat()
console.log(förnamn.concat(" ", efternamn)); // Output: "Anna Andersson"
// Kombinera flera strängar
console.log("Jag heter " + förnamn + " " + efternamn + " och jag är " + 25 + " år gammal."); 
// Output: "Jag heter Anna Andersson och jag är 25 år gammal."
```

Som du kan se i exemplet ovan kan man även kombinera strängar med andra datatyper, som i det sista exemplet där vi använder operatorn "+" för att sammanslå en sträng med en numerisk variabel.

## Djupdykning

När man använder operatorn "+" för att sammanfoga strängar i TypeScript så görs det automatisk konvertering av eventuella andra datatyper till strängar. Om det finns en annan datatyp än en sträng i strängsammanfogningen så kommer den datatypen att konverteras till en sträng. Det här kan vara användbart om man till exempel vill inkludera en variabel i en sträng men inte vill konvertera hela strängen till en variabel. 

För att göra koden ännu mer dynamisk när det gäller strängsammanfogningar kan man använda placeholders och template literals. Detta tillåter oss att inkludera variabler direkt i strängen utan att behöva använda operatorn "+" eller metoden "concat()". Här är ett exempel:

```TypeScript
let namn: string = "Lisa";
let ålder: number = 30;
// Placeholder med %
console.log("Mitt namn är %s och jag är %d år gammal.", namn, ålder);
// Output: "Mitt namn är Lisa och jag är 30 år gammal."
// Template literal med ${}
console.log(`Mitt namn är ${namn} och jag är ${ålder} år gammal.`);
// Output: "Mitt namn är Lisa och jag är 30 år gammal."
```

Som du kan se blir koden mer kompakt och lättläst med placeholders och template literals.

## Se även

- [TypeScript officiella dokumentation om strängar (engelska)](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Förstå skillnaden mellan operatorn "+" och metoden "concat()"](https://dmitripavlutin.com/differences-between-concat-and-operator-plus-operator-in-javascript/)