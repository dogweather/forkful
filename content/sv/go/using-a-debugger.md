---
title:                "Att använda en debugger"
date:                  2024-01-26T03:49:07.932916-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda en debugger"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/using-a-debugger.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att använda en felsökare är som att ha en GPS i kodens djungel; den guidar dig till problemets källa. Programmerare använder felsökare för att stega igenom sin kod, inspektera variabler och förstå flödet, vilket gör det enklare att fånga upp buggar och optimera prestanda.

## Hur man gör:
Go har ett inbyggt verktyg för felsökning som heter Delve (`dlv`). För att komma igång, installera Delve, skriv ett enkelt Go-program och kör det sedan genom felsökaren.

```Go
// Först, installera Delve
// go get -u github.com/go-delve/delve/cmd/dlv

// Exempel på Go-program, spara som main.go
package main

import "fmt"

func main() {
    message := "Felsökning med Delve!"
    fmt.Println(message)
}

// Kör ditt program med Delve
// dlv debug

// Några grundläggande Delve-kommandon:
// (dlv) break main.main // sätt en brytpunkt vid funktionen main
// (dlv) continue // kör tills brytpunkt eller programavslutning
// (dlv) step // enstegsgenomgång genom programmet
// (dlv) print message // skriv ut det aktuella värdet på variabeln 'message'
// (dlv) quit // avsluta Delve
```

Att köra `dlv debug` startar en felsökningssession. När du träffar en brytpunkt du har satt kan du stega igenom ditt program och se vad som pågår under huven.

## Fördjupning
Historiskt sett har Go-programmerare använt flera verktyg för felsökning såsom GDB (GNU Debugger) men stött på utmaningar eftersom GDB inte var skräddarsytt för Gos runtime och gorutiner. Delve kom till undsättning med bättre stöd för Gos unika funktioner.

Det finns alternativ till Delve som `go-dbg`, och även integrerat felsökningsstöd inom IDE:er som Visual Studio Code och GoLand, som använder Delve för en mer användarvänlig upplevelse.

På implementationsidan arbetar Delve genom att använda paketen `runtime` och `debug/gosym` bland andra, för att komma åt och tolka Go-programsymboler och körningsinformation. Det uppdateras ständigt för att hålla jämna steg med nya språkfunktioner och versioner.

## Se även
- Delves officiella repo: https://github.com/go-delve/delve
- Go Debugger-tutorial av Go-teamet: https://golang.org/doc/gdb
- Visual Studio Code Go-felsökning: https://code.visualstudio.com/docs/languages/go#_debugging
