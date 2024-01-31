---
title:                "Een debugger gebruiken"
date:                  2024-01-28T22:08:49.505710-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een debugger gebruiken"

category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een debugger gebruiken is als het hebben van een GPS in de jungle van code; het leidt je naar de bron van het probleem. Programmeurs gebruiken debuggers om door hun code te stappen, variabelen te inspecteren en de flow te begrijpen, waardoor het makkelijker wordt om bugs te vinden en de prestaties te optimaliseren.

## Hoe te:
Go heeft een ingebouwde tool voor debugging genaamd Delve (`dlv`). Om te beginnen, installeer Delve, schrijf een eenvoudig Go-programma en voer het vervolgens uit via de debugger.

```Go
// Eerst, installeer Delve
// go get -u github.com/go-delve/delve/cmd/dlv

// Voorbeeld Go programma, opslaan als main.go
package main

import "fmt"

func main() {
    message := "Debugging met Delve!"
    fmt.Println(message)
}

// Voer je programma uit met Delve
// dlv debug

// Enkele basis Delve commando's:
// (dlv) break main.main // zet een breakpoint bij functie main
// (dlv) continue // voer uit tot breakpoint of programma-terminatie
// (dlv) step // enkele stap door het programma
// (dlv) print message // print de huidige waarde van variabele 'message'
// (dlv) quit // verlaat Delve
```

Het uitvoeren van `dlv debug` start een debugging-sessie. Zodra je een breakpoint hebt bereikt dat je hebt ingesteld, kun je door je programma stappen en zien wat er onder de motorkap gebeurt.

## Diepere Duik
Historisch gezien hebben Go-programmeurs verschillende tools gebruikt voor debugging, zoals GDB (GNU Debugger), maar ondervonden uitdagingen omdat GDB niet was aangepast voor Go's runtime en goroutines. Delve kwam te hulp met betere ondersteuning voor Go's unieke kenmerken.

Er zijn alternatieven voor Delve zoals `go-dbg`, en zelfs geïntegreerde debuggerondersteuning binnen IDEs zoals Visual Studio Code en GoLand, die rondom Delve wikkelen voor een gebruiksvriendelijkere ervaring.

Aan de implementatiekant werkt Delve met de `runtime` en `debug/gosym` pakketten, onder andere, om toegang te krijgen tot en Go programma-symbolen en runtime-informatie te interpreteren. Het wordt constant bijgewerkt om bij te blijven met nieuwe taalfeatures en versies.

## Zie Ook
- Delve's Officiële Repo: https://github.com/go-delve/delve
- Go Debugger Tutorial door het Go Team: https://golang.org/doc/gdb
- Visual Studio Code Go Debugging: https://code.visualstudio.com/docs/languages/go#_debugging
