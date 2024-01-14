---
title:    "Go: Söka och ersätta text"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text i en kodbas är en vanligt förekommande uppgift för programmerare. Det kan bero på att variabelnamn behöver ändras för att bättre spegla det data som de håller, eller att en viktig funktion har blivit föråldrad och behöver ersättas med en modernare lösning. I denna blogginlägg kommer vi att titta på hur söka och ersätta text i Go-programmeringsspråket.

## Hur man gör det

För att söka och ersätta text i Go, använder vi oss av standardpaketeringen "strings" som innehåller funktioner för att hantera strängar. Den mest användbara funktionen för detta ändamål är "ReplaceAll", som tar tre argument: en sträng, ett sökord och ett ersättande ord. Enkelt uttryckt, så söker denna funktion igenom en given sträng och ersätter alla förekomster av det sökta ordet med det nya ordet.

Låt oss titta på ett exempel:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    text := "Hej på dig!"
    newText := strings.ReplaceAll(text, "Hej", "Hallå")

    fmt.Println(newText) // Output: Hallå på dig!
}
```

I detta exempel så skapar vi en variabel med strängen "Hej på dig!" och sedan använder vi ReplaceAll-funktionen för att ersätta "Hej" med "Hallå". Det nya värdet av variabeln skrivs ut och output blir då "Hallå på dig!".

Denna funktion kan också användas på mer komplicerade sökord, som reguljära uttryck. Genom att använda funktionen "ReplaceAllString" istället för "ReplaceAll", kan vi söka igenom texten med hjälp av reguljära uttryck och göra mer avancerade ersättningar.

## Djupdykning

För dig som vill lära dig mer om söka och ersätta text i Go, rekommenderar vi att titta närmare på "regexp" paketeringen. Denna paketering ger oss möjlighet att använda reguljära uttryck för att söka igenom och manipulera text på ett mer avancerat sätt.

Som en kuriositet, så introducerades stöd för reguljära uttryck i Go-språket redan vid version 1.0, vilket gör det till ett av de tidigaste språken att inkludera detta stöd.

## Se även

- [Go's official strings package](https://golang.org/pkg/strings/)
- [Go's official regexp package](https://golang.org/pkg/regexp/)
- [A practical guide to regular expressions in Go](https://www.calhoun.io/using-regexp-and-regular-expressions-in-go/)