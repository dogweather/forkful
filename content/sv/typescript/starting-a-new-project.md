---
title:    "TypeScript: Att börja ett nytt projekt"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Varför
Att starta ett nytt programmeringsprojekt är en spännande utmaning som ger möjligheten att skapa något unikt och användbart. Det är också ett bra sätt att träna och utveckla dina programmeringsfärdigheter.

## Hur man gör
För att starta ett nytt TypeScript-projekt behöver du först installera TypeScript via npm-kommandot `npm install -g typescript`. När det väl är installerat kan du skapa en ny mapp för ditt projekt och navigera till den i din terminal.

För att skapa en `package.json`-fil för projektet, använd kommandot `npm init`. Detta kommer att guida dig genom att skapa en `package.json`-fil som innehåller viktig information om ditt projekt.

Nu kan du använda TypeScript-kommandot `tsc` för att kompilera din kod. Till exempel, om du har en fil som heter `main.ts` som innehåller en enkel funktion som adderar två tal, kan du skriva följande kodblock i din `main.ts`-fil:

```TypeScript
function add(num1: number, num2: number): number {
  return num1 + num2;
}

console.log(add(2, 3));
```
Kör sedan kommandot `tsc main.ts` för att kompilera din kod. Detta kommer att generera en JavaScript-fil med samma namn som din TypeScript-fil.

När din kod är kompilerad, kan du köra den genom att skriva `node main.js` i terminalen. Detta kommer att ge dig följande utmatning:

```
5
```

## Djupdykning
När du börjar ditt TypeScript-projekt, är det viktigt att känna till de grundläggande koncepten i språket. TypeScript är ett objektorienterat språk som bygger på JavaScript, men med viktiga tillägg som typtolkning och klasser.

Du kommer också att behöva välja en lämplig textredigerare eller utvecklingsmiljö (IDE) för ditt projekt. Det finns många olika alternativ som VS Code, WebStorm och Atom som alla stöder TypeScript.

För att få en djupare förståelse för språket och dess funktioner kan du också kolla in dokumentationen och olika tutorials som finns tillgängliga online.

## Se även
- [Officiell TypeScript-dokumentation](https://www.typescriptlang.org/docs/)
- [TypeScript-tutorial för nybörjare](https://www.tutorialspoint.com/typescript/)
- [Skillshare-kurs: Getting Started with TypeScript](https://www.skillshare.com/classes/Getting-Started-with-TypeScript/1764476165)