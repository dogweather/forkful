---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:28.864624-07:00
description: "\xC5 bruke en debugger i Go-programmering involverer \xE5 benytte verkt\xF8\
  y eller funksjoner for \xE5 inspisere og endre tilstanden til et kj\xF8rende program\
  \ for \xE5\u2026"
lastmod: '2024-03-13T22:44:40.271837-06:00'
model: gpt-4-0125-preview
summary: "\xC5 bruke en debugger i Go-programmering involverer \xE5 benytte verkt\xF8\
  y eller funksjoner for \xE5 inspisere og endre tilstanden til et kj\xF8rende program\
  \ for \xE5\u2026"
title: "Bruke en feils\xF8ker"
---

## Hvordan:
Go tilbyr en innebygd fasilitet for feilsøking kalt `delve`. Det er et fullverdig feilsøkingsverktøy som lar deg utføre Go-programmer steg for steg, inspisere programvariabler og evaluere uttrykk.

For å starte, må du først installere `delve`. Dette kan du gjøre ved å kjøre:

```shell
go get -u github.com/go-delve/delve/cmd/dlv
```

Nå, la oss feilsøke et enkelt Go-program. Tenk på et program `main.go`:

```go
package main

import "fmt"

func main() {
    message := "Feilsøking i Go"
    fmt.Println(message)
}
```

For å starte feilsøkingen av dette programmet, åpne en terminal i prosjektets mappe og utfør:

```shell
dlv debug
```

Denne kommandoen kompilerer programmet med optimaliseringer deaktivert (for å forbedre feilsøkingsopplevelsen), starter det og legger til en debugger for det.

Når `delve` kjører, er du i det interaktive feilsøkingsskallet. Her er noen få grunnleggende kommandoer:

- `break main.main` setter et brytepunkt ved `main`-funksjonen.
- `continue` gjenopptar programutførelsen til et brytepunkt treffes.
- `print message` vil skrive ut verdien av `message`-variabelen.
- `next` flytter programutførelsen til neste linje.
- `quit` avslutter debuggeren.

Utdataen når brytepunktet treffes og variabelen blir skrevet ut, kan se slik ut:

```shell
Breakpoint 1 at 0x49ecf3 for main.main() ./main.go:6
> main.main() ./main.go:6 (treffer goroutine(1):1 totalt:1) (PC: 0x49ecf3)
     1: package main
     2:
     3: import "fmt"
     4:
     5: func main() {
     6: =>    message := "Feilsøking i Go"
     7:       fmt.Println(message)
     8: }
(dlv) print message
"Feilsøking i Go"
```

Ved å bruke disse kommandoene kan du stegvis gå gjennom programmet ditt, inspisere tilstanden underveis for å forstå hvordan det oppfører seg, og identifisere eventuelle problemer.

## Dypdykk
Valget av `delve` som Go sin foretrukne feilsøkingsverktøy fremfor tradisjonelle verktøy som GDB (GNU Debugger), skyldes hovedsakelig Go sin utførelsesmodell og kjøretid. GDB var ikke opprinnelig designet med Go-kjøretiden i tankene, noe som gjør `delve` til et mer passende valg for Go-utviklere. `Delve` er spesielt designet for Go og tilbyr en mer intuitiv feilsøkingsopplevelse for Go-rutiner, kanaler og andre Go-spesifikke konstruksjoner.

Videre støtter `delve` et bredt spekter av funksjoner utover det som tilbys av grunnleggende GDB når man jobber med Go-programmer. Disse inkluderer, men er ikke begrenset til: å feste seg til kjørende prosesser for feilsøking; betingede brytepunkter; og evaluering av komplekse uttrykk som kan involvere Go’s samtidighetsprimitiver.

Mens `delve` er den foretrukne debuggeren for mange Go-utviklere, er det verdt å merke seg at Go-verktøykjeden også inkluderer lettere former for feilsøkingsstøtte, som det innebygde `pprof`-verktøyet for profilering og `trace`-verktøyet for visualisering av samtidighet. Disse verktøyene kan noen ganger tilby en raskere eller mer høynivå-tilnærming til diagnostisering av ytelsesproblemer eller samtidighetsfeil, som kan være komplementære eller til og med foretrekkbare avhengig av feilsøkingskonteksten.
