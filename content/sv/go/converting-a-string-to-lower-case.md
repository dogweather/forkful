---
title:                "Go: Konvertera en sträng till gemener"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera en sträng till små bokstäver är en vanlig uppgift vid utveckling av program på olika programmeringsspråk. Det kan användas för att standardisera inmatade användardata eller jämföra strängar utan att behöva ta hänsyn till skillnader i bokstavsstorlek.

## Hur man gör
```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    // Definiera en sträng att konvertera
    str := "HeLLo wOrld"

    // Använd strings.ToLower() funktionen för att konvertera till små bokstäver
    lower := strings.ToLower(str)

    // Skriv ut resultatet
    fmt.Println(lower)

    // Output: hello world
}
```

## Djupdykning
Anledningen till att vi använder "strings.ToLower()" i exemplet ovan är för att det är den mest effektiva och tillförlitliga metoden för att konvertera strängar till små bokstäver på Go. Metoden hanterar även specialtecken och diakritiska tecken i andra språk, vilket är viktigt för globala applikationer.

Det är också värt att nämna att metoden returnerar en ny sträng istället för att ändra den existerande strängen. Detta är en viktig del av Go's designfilosofi där det anses vara mindre felbenäget att återanvända existerande variabler istället för att modifiera dem direkt.

## Se även
- [Strings Package i Go](https://golang.org/pkg/strings/)
- [Strings.ToLower() funktionen i Go](https://golang.org/pkg/strings/#ToLower)