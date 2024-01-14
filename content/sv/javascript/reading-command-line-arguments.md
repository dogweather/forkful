---
title:    "Javascript: Läsning av kommandoradsargument"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Varför

Programmering kan vara en spännande och utmanande hobby eller karriär. Men ibland kan man stöta på problem som kräver lite extra kunskap. Att läsa in kommandoradsargument är en av de färdigheter som kan vara mycket användbara när man arbetar med Javascript-programmering. Genom att lära sig detta kan man enkelt interagera med användaren och göra sina program mer interaktiva och användarvänliga. Så varför inte fördjupa sig i detta ämne och lära sig hur man gör?

## Hur man gör

Kommandoradsargument, eller command line arguments på engelska, är parametrar som ges till ett program när det körs via kommandoraden. Det kan till exempel vara ett värde som programmeraren vill använda i sin kod eller en flagga som styr programmet. Att läsa in dessa argument i Javascript är enkelt med hjälp av process.argv-objektet.

För att läsa in kommandoradsargument i Javascript skriver man följande kod:

```
process.argv[2] // detta är indexet för det första argumentet
```

Om man till exempel kör programmet med argumentet "node program.js Hej!", så kommer det första argumentet att vara "Hej!" och det kommer att lagras på index 2 i process.argv-objektet. Detta kan man sedan använda i sin kod på olika sätt beroende på vad man behöver. Här är ett exempel på hur man kan skriva ut argumentet som programmet tar emot:

```
console.log("Hej " + process.argv[2] + "!");
```

När man kör detta program med argumentet "node program.js Svenska läsare", så kommer outputen att vara "Hej svenska läsare!".

## Djupdykning

Det finns många möjligheter och användningsområden för att läsa in kommandoradsargument i Javascript. Ofta används det för att ge programmet olika inställningar eller för att interagera med användaren. Det finns också sätt att hantera flera argument eller att kontrollera om ett argument är en flagga eller ett värde. Ett annat sätt att använda detta är att dynamiskt generera kod baserat på kommandoradsargumenten som programmet tar emot.

Det är också viktigt att ta hänsyn till säkerheten när man läser in kommandoradsargument. Det är viktigt att sanitizera, eller rensa, input från kommandoradsargumenten för att undvika säkerhetsrisker som till exempel SQL-injektioner.

## Se också

- [Node.js Process Dokumentation](https://nodejs.org/api/process.html)
- [Användning av Kommandoradsargument i Enkla Program](https://code.visualstudio.com/docs NodeJS/nodejs-debugging#_command-line-arguments)
- [Sanitization av Input i Node.js](https://www.javascripting.com/view/sanitize)

Tack för att du läste! Fortsätt utforska och utveckla dina programmeringsfärdigheter genom att lära dig nya saker som att läsa in kommandoradsargument. Det kan bidra till att göra dina program mer dynamiska och användarvänliga. Lycka till!