---
title:                "Een debugger gebruiken"
date:                  2024-02-03T18:10:12.372901-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een debugger gebruiken"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/using-a-debugger.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Bij het programmeren in Go gebruikmaken van een debugger houdt in dat je tools of functies gebruikt om de staat van een draaiend programma te inspecteren en te wijzigen, om zo het gedrag ervan te begrijpen of problemen op te sporen. Programmeurs doen dit om efficiënt bugs te vinden en op te lossen, de prestaties te optimaliseren en de correctheid van hun code te waarborgen.

## Hoe te:

Go biedt een ingebouwde faciliteit voor debugging genaamd `delve`. Het is een volledig uitgerust debugginggereedschap waarmee je Go-programma's stap voor stap kunt uitvoeren, programmavariabelen kunt inspecteren en uitdrukkingen kunt evalueren.

Om te beginnen, moet je eerst `delve` installeren. Dit kun je doen door uit te voeren:

```shell
go get -u github.com/go-delve/delve/cmd/dlv
```

Laten we nu een eenvoudig Go-programma debuggen. Beschouw een programma `main.go`:

```go
package main

import "fmt"

func main() {
    message := "Debuggen in Go"
    fmt.Println(message)
}
```

Om dit programma te debuggen, open je een terminal in de map van het project en voer je uit:

```shell
dlv debug
```

Dit commando compileert het programma met optimalisaties uitgeschakeld (om de debugervaring te verbeteren), start het en koppelt een debugger eraan.

Eenmaal `delve` draait, bevind je je in de interactieve debuggershell. Hier zijn een paar basale commando's:

- `break main.main` zet een breekpunt bij de `main`-functie.
- `continue` hervat de uitvoering van het programma totdat een breekpunt wordt geraakt.
- `print message` drukt de waarde van de `message`-variabele af.
- `next` gaat verder met de uitvoering van het programma naar de volgende regel.
- `quit` verlaat de debugger.

De uitvoer bij het raken van het breekpunt en het afdrukken van de variabele ziet er misschien als volgt uit:

```shell
Breakpoint 1 at 0x49ecf3 for main.main() ./main.go:6
> main.main() ./main.go:6 (hits goroutine(1):1 total:1) (PC: 0x49ecf3)
     1: package main
     2:
     3: import "fmt"
     4:
     5: func main() {
     6: =>    message := "Debuggen in Go"
     7:       fmt.Println(message)
     8: }
(dlv) print message
"Debuggen in Go"
```

Met behulp van deze commando's kun je stapsgewijs door je programma gaan, de staat inspecteren terwijl je verder gaat om te begrijpen hoe het zich gedraagt, en eventuele problemen identificeren.

## Diepgaand

De keuze voor `delve` als het debuggereedschap bij uitstek voor Go, boven traditionele gereedschappen zoals GDB (GNU Debugger), komt voornamelijk vanwege de aard van Go's uitvoeringsmodel en runtime. GDB was oorspronkelijk niet ontworpen met de Go-runtime in gedachten, wat `delve` een geschiktere keuze maakt voor Go-ontwikkelaars. `Delve` is specifiek ontworpen voor Go en biedt een intuïtievere debug-ervaring voor Go-routines, kanalen en andere Go-specifieke constructies.

Verder ondersteunt `delve` een breed scala aan functies die verder gaan dan wat de basis GDB biedt bij het werken met Go-programma's. Deze omvatten, maar zijn niet beperkt tot: het hechten aan lopende processen voor debugging; conditionele breekpunten; en het evalueren van complexe uitdrukkingen die mogelijk Go's concurrency-primitieven omvatten.

Hoewel `delve` de goto-debugger is voor veel Go-ontwikkelaars, is het de moeite waard om te vermelden dat de Go-gereedschapsketen ook lichtgewichtere vormen van debug-ondersteuning bevat, zoals het ingebouwde `pprof`-gereedschap voor profilering en het `trace`-gereedschap voor visualisatie van concurrency. Deze gereedschappen kunnen soms een snellere of meer hoogwaardige avenue bieden voor het diagnosticeren van prestatieproblemen of concurrency-bugs, die complementair kunnen zijn of zelfs de voorkeur kunnen hebben, afhankelijk van de debug-context.
