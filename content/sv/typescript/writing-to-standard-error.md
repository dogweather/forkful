---
title:                "Skriva till standardfel"
html_title:           "TypeScript: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standardfel är en viktig del av felsökning i programmering. Genom att skriva felmeddelanden till standardfel, eller "stderr", kan du få mer detaljerad information om vad som går fel i ditt program. Detta kan hjälpa dig att identifiera problemet och åtgärda det mer effektivt.

## Såhär gör du

För att skriva till standardfel i TypeScript, kan du använda det inbyggda objektet "process.stderr". Detta objekt har en metod som heter "write" som låter dig skriva text till standardfel. Exempelkod nedan visar hur man använder denna metod:

```TypeScript
process.stderr.write("Det här är ett felmeddelande!\n");
```

Detta kommer att skriva ut texten "Det här är ett felmeddelande!" till standardfel och lägga till en ny rad efteråt. 

En annan användbar metod i "process.stderr" är "writeln", som automatiskt lägger till en ny rad efter den skrivna texten. Exempelkod nedan:

```TypeScript
process.stderr.writeln("Det här är ett felmeddelande!");
```

Detta kommer att producera samma resultat som första exemplet, men du behöver inte lägga till "\n" för att lägga till en ny rad.

## Djupdykning

När du skriver till standardfel är det viktigt att du förstår hur datatypen är formaterad. Standardfel är ett "stream" objekt, vilket betyder att det är en ström av data som skickas ut i realtid. Detta är annorlunda än en vanlig sträng som bara är en statisk data.

Ett problem som kan uppstå när du skriver till standardfel är att felmeddelandet inte visas när du förväntar dig det. Detta kan bero på att standardfel är en ström och om din kod exekveras för snabbt, kan felmeddelandet inte hinnas visas innan programmet avslutas. För att lösa detta kan du använda metoden "process.stderr.end()", vilket tvingar programmet att vänta tills all data i standardfel har skrivits ut innan det avslutas.

## Se även

- [Node.js dokumentation om process.stderr](https://nodejs.org/api/process.html#process_process_stderr)
- [En guide för felsökning i Node.js](https://medium.com/intrinsic/node-js-debugging-for-beginners-83cb5fdd1953)