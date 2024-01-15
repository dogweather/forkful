---
title:                "Läsning av kommandoradsargument"
html_title:           "Javascript: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Om du någonsin har använt ett program från en terminal, har du förmodligen stött på begreppet "kommandoradsargument" eller "kommandoradsparametrar". Det är helt enkelt information som kan ges till ett program när det körs för att påverka dess beteende. Men varför är det viktigt att kunna läsa dessa argument? Det kan vara till stor hjälp när du vill anpassa eller manipulera ditt program för att passa dina specifika behov.

## Hur man gör

För att läsa kommandoradsargument i Javascript, använder du process.argv-objektet som innehåller en array av argument som skickas till ditt program. Låt oss titta på ett enkelt exempel där vi vill visa ett meddelande beroende på vilka argument som skickats till programmet.

```javascript
// Kör programmet med "node arguments.js hello world"
// node arguments.js är namnet på programfilen och "hello" och "world" är argumenten som skickas med

// arguments.js
const argument = process.argv[2];
console.log("Hej " + argument + "!");
// Output: Hej hello!
```

Det här är en mycket grundläggande metod, men den ger en bra grund för att förstå hur man läser kommandoradsargument. Genom att referera till rätt index i process.argv-arrayen kan du få tillgång till specifika argument som skickats till ditt program.

## Utforska djupare

Nu när du har en förståelse för hur man läser kommandoradsargument, kan du utforska djupare för att lära dig mer avancerade tekniker och användningar. Ett användbart tillämpningsområde är att skapa mer flexibla och anpassningsbara program. Du kan också använda kommandoradsargument för att implementera enkla användargränssnitt för dina program, vilket gör det lättare för användare att interagera med dem.

## Se även

För mer information och exempel, se till exempel följande länkar:

- [Node.js process.argv dokumentation](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Codecademy tutorial om kommandoradsargument i Javascript](https://www.codecademy.com/articles/command-line-arguments-javascript)
- [Kommandoradsargument: Vad de är och hur man använder dem](https://www.digitalocean.com/community/tutorials/how-to-read-command-line-arguments-in-node-js)