---
title:                "TypeScript: Att göra en sträng stor"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna konvertera text till versaler är en grundläggande funktion som är användbar för många olika programmeringsproblem. Genom att förstå hur man kapitaliserar en sträng kan du enkelt implementera det i dina projekt för att skapa mer professionella och enhetliga utskrifter.

## Hur man gör

För att konvertera en sträng till versaler i TypeScript kan du använda inbyggda funktionen `toUpperCase()` som är tillgänglig för alla strängar. Se nedan för ett exempel:

```TypeScript
let sträng = "detta är en sträng som ska kapitaliseras";
console.log(sträng.toUpperCase());
```

Detta kommer att ge följande utskrift:

```
DET ÄR EN STRÄNG SOM SKA KAPITALISERAS
```

Det är också möjligt att konvertera en specifik del av en sträng genom att använda `toUpperCase()` tillsammans med `substring()` funktionen. Se exemplet nedan:

```TypeScript
let sträng = "detta är en sträng som ska kapitaliseras";
let delAvSträng = sträng.substring(20, 31).toUpperCase();
console.log("Kapitaliserad del av strängen: " + delAvSträng);
```

Detta kommer att ge följande utskrift:

```
KAPITALISERAD DEL AV STRÄNGEN: SKA KAPITAL
```

## En djupdykning

När `toUpperCase()` funktionen används på en sträng, skapar den en ny sträng istället för att modifiera den befintliga. Detta kan till en början verka onödigt men är faktiskt en viktig del av "immuniseringsprincipen" i programmering. Detta innebär att det är säkrare att inte göra förändringar på befintliga variabler eftersom det kan leda till oförutsägbara resultat.

En annan intressant sak att notera är att `toUpperCase()` funktionen endast konverterar tecken som är bokstäver i det aktuella språket. Till exempel kommer den inte att förändra siffror eller specialtecken.

## Se även

- [Lär dig TypeScript på 5 minuter](https://medium.com/better-programming/learn-typescript-in-5-minutes-13ea11dc2c0a)
- [Effektiva programmeringsvanor för att undvika buggar](https://blog.usejournal.com/effective-programming-habits-to-avoid-bugs-a86a88f3112a)
- [Officiell Typescript-dokumentation](https://www.typescriptlang.org/docs/home.html)