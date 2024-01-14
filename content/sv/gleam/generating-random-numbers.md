---
title:    "Gleam: Generera slumpmässiga nummer"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är ett avgörande verktyg för att skapa variation i dina program och göra dem mer mångsidiga. Det är också användbart i spel och för att testa program.

## Hur man gör det

Att generera slumpmässiga nummer är väldigt enkelt med Gleam. Använd funktionen `random.int` för att generera ett slumpmässigt heltal. Här är ett exempel:

```Gleam
let random_number = random.int(1, 10)
```

Detta kodblock kommer att generera ett slumpmässigt heltal mellan 1 och 10 och tilldela det till variabeln `random_number`. Du kan sedan använda detta värde i ditt program.

Om du vill generera ett slumpmässigt flyttal, använd funktionen `random.float` på samma sätt som `random.int`.

För att säkerställa att ditt program alltid genererar nya slumpmässiga nummer, kan du använda funktionen `random.seed` för att sätta en startpunkt för slumpmässiga nummergenereringen.

## Djupdykning

I Gleam finns det flera olika funktioner för att generera slumpmässiga nummer som passar olika behov. `random.int`, `random.float` och `random.seed` är bara några av dem. Det finns också funktioner för att generera slumpmässiga teckensträngar och till och med slumpmässiga booleska värden.

En annan intressant funktion är `random.pick` som väljer ett slumpmässigt element från en given lista eller tupel. Detta kan vara användbart för att skapa variation i dina program.

## Se även

Här är några länkar till Gleam-dokumentationen om generering av slumpmässiga nummer:
- https://gleam.run/book/libraries/random
- https://gleam.run/book/libraries/random-float
- https://gleam.run/book/libraries/random-string
- https://gleam.run/book/libraries/random-pick