---
title:                "TypeScript: Att få dagens datum"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna få den nuvarande datumen är ett vanligt förekommande behov i många programmeringsprojekt. Det kan användas för att visa aktuell tid och datum på en webbsida, för att sortera och filtrera data baserat på datum eller för att skapa dynamiska tidsstämplar i en applikation.

## Hur man

För att få den nuvarande datumen i TypeScript, kan man använda Date-objektet och dess metoder. Nedan är ett exempel på hur man enkelt kan få den nuvarande datumen och tidsstämpeln:

```TypeScript
let nu = new Date(); //Skapar ett nytt Date-objekt
let dag = nu.getDate(); //Få dagens datum (1-31)
let manad = nu.getMonth() + 1; //Få månadens nummer (0-11, därför lägger vi till 1)
let ar = nu.getFullYear(); //Få årtal (ex. 2021)
let tid = nu.getHours() + ":" + nu.getMinutes(); //Få aktuell tid (timmar:minuter)

console.log("Idag är det " + dag + "/" + manad + "/" + ar + " och klockan är " + tid);
```

Detta kodexempel kommer att ge utskriften "Idag är det 8/5/2021 och klockan är 13:30". Genom att använda olika metoder på Date-objektet, som getDate(), getMonth(), getFullYear() och getHours(), kan man få olika delar av den nuvarande datumen och tidsstämpeln.

## Djupdykning

Date-objektet i JavaScript/TypeScript är baserat på det så kallade Unix-tiden eller Unix-epoken. Detta är ett system som räknar tiden från midnatt den 1 januari 1970 UTC (koordinerad universell tid). Varje millisekund sedan dess har ett unikt nummer, vilket är vad Date-objektet använder för att beräkna datum och tid.

Det är också värt att notera att när man skapar ett Date-objekt utan några argument, kommer det att använda nuvarande locale (språk och tidszon) på användarens dator. Detta kan orsaka problem om man till exempel delar koden med någon från en annan del av världen som har en annan locale.

## Se också

Här är några andra resurser för att lära dig mer om att få den nuvarande datumen i TypeScript:

- [W3Schools - Date Object](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [MDN web docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) 
- [TypeScript Handbook - Date](https://www.typescriptlang.org/docs/handbook/datetime.html#date-objects)

Tack för att du läste!