---
title:    "TypeScript: Kontrollera om en mapp finns"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Varför

När vi skriver kod, finns det många fall där vi behöver kontrollera om en mapp finns innan vi fortsätter med vår logik. Det kan vara för att skapa en ny mapp, läsa från en befintlig mapp eller helt enkelt för att säkerställa att vår kod fungerar korrekt. I denna blogginlägg, kommer vi att djupdyka i hur man på ett enkelt sätt kan kontrollera om en mapp finns med TypeScript.

## Hur man gör det

För att kontrollera om en mapp finns i TypeScript, används Node.js fs modulen. Den tillhandahåller en metod som heter `existsSync`som returnerar `true` om mappen finns, annars returnerar den `false`. 

```TypeScript
import fs = require('fs');

if(fs.existsSync("./mappen")){
    console.log("Mappen finns redan!");
} else {
    console.log("Mappen finns inte.");
}

```

Output: 

```
Mappen finns inte.
```

Det första vi gör är att importera `fs`modulen från Node.js och använder sedan `existsSync`metoden för att kontrollera om en mapp med namnet "mappen" finns i vår projektmapp. Om mappen finns, kommer vi att få utskriften "Mappen finns redan", annars kommer vi att få utskriften "Mappen finns inte". 

Det är viktigt att notera att `existsSync`metoden tar in en absolut sökväg eller en relativ sökväg till målmappen som ett argument. I exemplet ovan har vi använt en relativ sökväg genom att ange `./mappen`, vilket betyder att mappen ska finnas i samma mapp som vår körfil. Om du vill kontrollera en mapp i en annan plats, måste du ange den absoluta sökvägen till mappen istället. 

En annan viktig punkt att komma ihåg när du använder `existsSync`metoden är att den är synkron, vilket betyder att koden kommer att vänta på att kontrollen ska slutföras innan den fortsätter med resten av logiken. Om du vill ha en asynkron kontroll, kan du använda `exists`metoden. 

## Djupdykning

`existsSync`metoden är mycket användbar när det gäller att kontrollera mappar i TypeScript, men det finns några saker att tänka på för att få den att fungera korrekt. 

•	Se till att den relativa eller absoluta sökvägen till mappen som du kontrollerar finns på rätt plats. Om du till exempel bara anger mappen som "mappen" men den egentligen befinner sig i en undermapp, kommer metoden att returnera `false` även om mappen faktiskt finns i projektet. 

•	För att undvika problem med att sökvägen inte hittas, är det en bra idé att använda Node.js `path`modulen för att skapa korrekta sökvägar. Så, istället för att manuellt ange sökvägen till mappen, kan du använda `path.join()`metoden för att bygga sökvägen baserat på den aktuella operativsystemet. 

•	Se till att du har de nödvändiga behörigheterna för att kontrollera en mapp. Om du försöker kontrollera en mapp som du inte har åtkomst till, kommer `existsSync`metoden att kasta ett fel.

## Se också

• [Node.js fs modulen](https://nodejs.org/api/fs.html) 
• [Node.js path modulen](https://nodejs.org/api/path.html) 
• [Mappstruktur i TypeScript](https://www.typescriptlang.org/docs/handbook/module-resolution.html#folder-structure) 

```