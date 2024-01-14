---
title:                "Gleam: Utskrift av felanalys"
simple_title:         "Utskrift av felanalys"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

# Varför
En av de viktigaste delarna av programmering är att kunna felsöka koden. Att kunna skriva ut debug-utmatning är ett värdefullt verktyg för att förstå vad som händer i koden och hitta eventuella fel. Det är en enkel och effektiv metod som kan användas under utvecklingsprocessen.

## Hur man gör
För att skriva ut debug-utmatning i Gleam kan du använda funktionen `debug.print` och ange det du vill skriva ut som argument. Till exempel, om du vill skriva ut en sträng, kan du använda följande kod:

```Gleam
let str = "Hej värld!"
debug.print(str)
```

Detta kommer att skriva ut "Hej värld!" i terminalen eller på annan utmatningsplats, beroende på hur du kör koden.

Om du vill ha ännu mer detaljerad information kan du använda `debug.print_all` som tar emot ett element och skriver ut alla dess egenskaper. Du kan också använda `debug.inspect` för att få en formaterad utskrift av ett element, vilket kan vara särskilt användbart för att felsöka komplexa datastrukturer.

## Deep Dive
För att förstå hur debug-utmatning fungerar i Gleam, är det viktigt att förstå att det är en del av det inbyggda biblioteket `gleam_debug`. Detta bibliotek ger funktioner för att skriva ut text, nummer, booleska värden och andra typer av data.

En annan intressant funktion i `gleam_debug` är `debug.trace`, som skriver ut information om varje körningsvarv i koden. Detta kan vara användbart för att förstå vilka delar av koden som har utförts och i vilken ordning.

Så använd debug-utmatning för att felsöka din Gleam-kod och hitta eventuella problem!

# See Also
Här är några användbara resurser för att lära dig mer om debugging i Gleam:

- Officiell Gleam dokumentation om felsökning: https://gleam.run/book/tips-and-tricks.html#debugging
- En guide till hur man felsöker i Gleam: https://medium.com/gleam-lang/another-guide-to-debugging-with-gleam-65ac455ca02e
- GitHub-repository för `gleam_debug`: https://github.com/gleam-lang/gleam_debug