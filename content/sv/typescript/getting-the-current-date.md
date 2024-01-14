---
title:    "TypeScript: Hämta aktuellt datum"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Varför

Att kunna få den nuvarande datumen i ett programmeringsprojekt kan vara mycket användbart i många olika situationer. Det kan hjälpa till att hålla koll på tidsfrister, planera scheman och generera dynamiska innehåll baserat på den aktuella datumen.

## Hur man gör

För att få den aktuella datumen i TypeScript, kan man använda den inbyggda Date-klassen. Här är en kodexempel:

```TypeScript
let today = new Date();
console.log(today);
```

Detta kommer att skapa en Date-objektet som representerar nuvarande datum och tid. Om du bara vill ha datumet, kan du använda `getDate()` metoden:

```TypeScript
let date = today.getDate();
console.log(date);
```

Detta kommer att skriva ut det aktuella datumet. Du kan också formatera datumet på olika sätt enligt dina behov med hjälp av andra metoder som `getMonth()` och `getFullYear()`.

## Djupdykning

I bakgrunden använder Date-klassen Unix-tidsstämpel för att representera datum och tid. Detta är ett värde som motsvarar antalet millisekunder som har passerat sedan 1 januari 1970 kl. 00:00:00 UTC. Med hjälp av detta kan du också spara och hantera datum i din databas eller skicka dem i en API-förfrågan.

Ett annat viktigt koncept som är värt att nämna när det gäller datum och tidshantering är tidszoner. Om du behöver visa datum och tid för en viss tidszon, måste du också inkludera ett tidszonobjekt vid instansiering av Date-klassen.

## Se även

- [Date-klassen i TypeScript dokumentationen](https://www.typescriptlang.org/docs/handbook/classes.html#date)
- [Datum och tidshantering i JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)