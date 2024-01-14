---
title:                "TypeScript: Sammanslagning av strängar"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanslå strängar är en vanlig uppgift vid programmering. Detta innebär att man kombinerar flera enskilda strängar till en enda längre sträng. Genom att göra detta kan man skapa mer dynamiska och mångsidiga program som kan generera olika typer av text baserat på användarens interaktion eller input. Detta kan vara användbart vid till exempel skapande av användargränssnitt eller generering av rapporter.

## Hur man gör

I TypeScript finns det flera sätt att sammanslå strängar, men det vanligaste är genom användning av + operatören. Detta fungerar på samma sätt som i många andra programmeringsspråk där man helt enkelt lägger ihop strängarna.

```TypeScript
let förnamn: string = "Lisa";
let efternamn: string = "Andersson";

console.log(förnamn + " " + efternamn);
```

Output:

```
Lisa Andersson
```

Man kan också använda inbyggda metoder för att sammanslå strängar, till exempel `concat()` och `join()`. Dessa metoder ger mer flexibilitet och möjlighet att sammanslå flera strängar samtidigt.

```TypeScript
let vem: string = "Jag";
let aktivitet: string = "gillar att programmera";

console.log(vem.concat(" ", aktivitet));
console.log([vem, aktivitet].join(" "));
```

Output:

```
Jag gillar att programmera
Jag gillar att programmera
```

Det finns också tillfällen då man behöver inkludera variabler eller andra specifika värden i en sträng, och detta kan göras genom användning av template literals. Dessa tillåter att man kan använda variabler och uttryck inom en sträng utan att behöva använda + operatören.

```TypeScript
let poäng: number = 85;

console.log(`Grattis, du fick ${poäng} poäng!`);
```

Output:

```
Grattis, du fick 85 poäng! 
```

## Djupdykning

Vid sammanslagning av strängar är det viktigt att tänka på hur man effektivt hanterar minnesallokering och resursanvändning. Vid tillfällen då man sammanslår stora mängder strängar eller utför operationen ofta kan det vara mer effektivt att använda en datastruktur som `Array` istället för att ändra på befintliga strängar. Detta kan också göra koden mer läsbar och mindre rörlig.

## Se också

- [TypeScript dokumentation om strängar](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Visual Studio Code Merge Strings Extension](https://marketplace.visualstudio.com/items?itemName=voxual.vsc-merge-strings)
- [Artikel om optimering av strängsammanslagning](https://www.codeproject.com/Articles/70950/Optimized-String-Concatenation-in-C)