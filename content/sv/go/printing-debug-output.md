---
title:    "Go: Utskrift av felsökningsresultat"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva ett program kan vara roligt, men ibland kan det också vara frustrerande när du stöter på fel eller buggar. Och det är där utskrift av felsökningsinformation kommer in i bilden. Genom att skriva ut debug-utmatning får du bättre förståelse för vad som händer i ditt program och kan effektivt hitta och lösa problem.

## Hur man gör

För att skriva ut debug-utmatning i Go kan du använda sig av funktionen "fmt.Printf()" eller "fmt.Println()". Dessa funktioner gör det möjligt för dig att skriva ut värden för olika variabler i ditt program. Låt oss ta ett exempel:

```
package main

import "fmt"

func main() {
    nummer := 10
    namn := "Johan"

    fmt.Printf("Värdet av variabeln nummer är: %d\n", nummer)
    fmt.Println("Namnet på användaren är: ", namn)
}
```

I detta exempel skriver vi ut värdet av variabeln "nummer" genom att använda funktionen "fmt.Printf()" och vi skriver ut värdet av variabeln "namn" genom att använda funktionen "fmt.Println()". Utmatningen för detta program kommer att vara:

```
Värdet av variabeln nummer är: 10
Namnet på användaren är: Johan
```

Notera att vi har använt "%d" för att representera värdet av "nummer" och inte "%s" som är den vanliga sträng-ersättaren. Det är viktigt att förstå skillnaden mellan de olika ersättarna för att kunna skriva ut korrekt information.

## Deep Dive

Som du kanske har märkt kan du använda dig av flera "fmt" funktioner för att skriva ut variabler i ditt program. Några andra användbara funktioner är "fmt.Sprintf()" som gör det möjligt för dig att formatera en sträng och spara den i en variabel och "fmt.Fprintf()" som skriver ut till en IO-strömm eller fil istället för till standardutmatningen.

En viktig aspekt att tänka på när du skriver ut debug-utmatning är att ta hänsyn till prestanda. Om ditt program är mycket stort och du skriver ut massor av information till din terminal eller fil kan det påverka prestandan negativt. Det är därför viktigt att endast skriva ut den information som behövs och att ta bort utskrifterna när de inte längre behövs.

## Se även

* [Officiell dokumentation för fmt-paketet på Go's webbplats](https://golang.org/pkg/fmt/)
* [Detaljerad guide om debug-utmatning i Go](https://www.digitalocean.com/community/tutorials/how-to-debug-go-code)
* [Go's community forum för support och diskussioner](https://forum.golangbridge.org/)