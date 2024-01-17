---
title:                "Utskrift av felsökningsutdata"
html_title:           "Go: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Debugging handlar om att söka och åtgärda buggar eller fel i din kod. Ibland kan det vara svårt att förstå varför en kod inte fungerar som den ska, och det är då man behöver hjälp för att hitta problemet och lösa det. Genom att skriva ut debug information kan du som programmerare enkelt följa hur din kod beter sig och identifiera felaktiga eller oönskade resultat.

## Så här:
Att skriva ut debug information i Go är enkelt. Du kan använda funktionen fmt.Println() för att skriva ut en sträng, variabel eller annan data till terminalen eller kommandotolken. Här är ett exempel på hur du kan göra det:

```Go
package main

import "fmt"

func main() {
   name := "Elin"
   fmt.Println("Hej", name, "välkommen till Go programmering!")
}
```

När du kör programmet ska det visa följande output: "Hej Elin, välkommen till Go programmering!"

Du kan också använda funktionen fmt.Printf() för mer detaljerade utskrifter. Detta tillåter dig att specificera formateringssträngar för olika datatyper. Här är ett exempel på hur du kan använda det:

```Go
package main

import "fmt"

func main() {
   age := 25
   fmt.Printf("Min ålder är %d år.", age)
}
```

Outputen kommer att vara: "Min ålder är 25 år."

## Djupdykning:
Debugging är en viktig del av programmering och har funnits sedan början av datorer. I äldre programmeringsspråk, som C och C++, användes vanligtvis funktioner som printf() och fprintf() för att skriva ut debug information. I dagens moderna språk som Go finns det inbyggda funktioner för att enkelt skriva ut debug information.

Alternativ till att skriva ut debug information är att använda debuggers eller att lägga till loggningsfunktionalitet i din kod. Men att skriva ut debug information är en snabb och enkel metod som fortfarande är användbar och populär bland programmerare.

Go har också en mängd inbyggda funktioner som kan hjälpa till vid debugging, som t.ex. runtime package som ger åtkomst till information om ditt program och dess körexekvering.

## Se även:
- [Go language specification](https://golang.org/ref/spec) - officiell specifikation för Go språket.
- [Go tour](https://tour.golang.org/welcome/1) - ett interaktivt sätt att lära dig Go programmering.
- [The Go Blog](https://blog.golang.org/) - offentlig blogg för nyheter och uppdateringar om Go.