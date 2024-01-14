---
title:                "Gleam: Att hitta längden på en sträng."
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att kunna hitta längden på en sträng är en viktig och grundläggande färdighet inom programmering. Det kan hjälpa till att optimera kod och lösa komplexa problem.

## Hur man gör det
För att hitta längden på en sträng i Gleam, kan du använda funktionen ```String.length()``` som returnerar antalet tecken i en given sträng. Här är ett enkelt exempel:

```Gleam
let str = "Hej, detta är en exempelsträng!"
let length = String.length(str)
io.format("Längden på strängen är: {}", [length])
```

Output:
```
Längden på strängen är: 30
```

Du kan också använda en loop för att iterera över varje tecken i strängen och öka en räknare för varje tecken tills du når slutet av strängen. Detta kan vara en användbar metod om du behöver göra ytterligare beräkningar eller manipulation på varje tecken.

## Djupdykning
I Gleam är strängar representerade som listor av Unicode-tecken. Detta innebär att längden på en sträng kan beräknas genom att räkna antalet element i denna lista.

En annan intressant aspekt är att Gleam tillåter dig att använda öppna typer, vilket innebär att du kan ha en funktion som tar emot en sträng utan att specificera längden på strängen i förväg. Istället kommer längden att bestämmas vid kompilering baserat på input som matas in i funktionen.

## Se även
- [Gleam - Officiell webbplats (på engelska)](https://gleam.run/)
- [Gleam på GitHub (på engelska)](https://github.com/gleam-lang/gleam)
- [Officiell Gleam-dokumentation (på engelska)](https://gleam.run/book/)