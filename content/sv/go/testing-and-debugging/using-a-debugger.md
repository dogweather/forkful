---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:09.034247-07:00
description: "Go erbjuder en inbyggd m\xF6jlighet f\xF6r fels\xF6kning som kallas\
  \ `delve`. Det \xE4r ett fullfj\xE4drat fels\xF6kningsverktyg som till\xE5ter dig\
  \ att exekvera Go-program\u2026"
lastmod: '2024-03-13T22:44:37.397025-06:00'
model: gpt-4-0125-preview
summary: "Go erbjuder en inbyggd m\xF6jlighet f\xF6r fels\xF6kning som kallas `delve`.\
  \ Det \xE4r ett fullfj\xE4drat fels\xF6kningsverktyg som till\xE5ter dig att exekvera\
  \ Go-program\u2026"
title: "Att anv\xE4nda en debugger"
weight: 35
---

## Hur man gör:
Go erbjuder en inbyggd möjlighet för felsökning som kallas `delve`. Det är ett fullfjädrat felsökningsverktyg som tillåter dig att exekvera Go-program steg för steg, inspektera programvariabler och utvärdera uttryck.

För att börja måste du först installera `delve`. Detta kan du göra genom att köra:

```shell
go get -u github.com/go-delve/delve/cmd/dlv
```

Nu, låt oss felsöka ett enkelt Go-program. Betrakta ett program `main.go`:

```go
package main

import "fmt"

func main() {
    message := "Debugging in Go"
    fmt.Println(message)
}
```

För att börja felsöka detta program, öppna en terminal i projektets katalog och exekvera:

```shell
dlv debug
```

Detta kommando kompilerar programmet med optimeringar inaktiverade (för att förbättra felsökningsupplevelsen), startar det, och kopplar en felsökare till det.

När `delve` körs, är du i det interaktiva felsökningsskalet. Här är några grundläggande kommandon:

- `break main.main` sätter en brytpunkt vid funktionen `main`.
- `continue` återupptar programexekveringen till dess att en brytpunkt träffas.
- `print message` skriver ut värdet på variabeln `message`.
- `next` framskrider programexekveringen till nästa rad.
- `quit` avslutar felsökaren.

Utskriften när brytpunkten träffas och variabeln skrivs ut kan se ut så här:

```shell
Breakpoint 1 at 0x49ecf3 for main.main() ./main.go:6
> main.main() ./main.go:6 (hits goroutine(1):1 total:1) (PC: 0x49ecf3)
     1: package main
     2:
     3: import "fmt"
     4:
     5: func main() {
     6: =>    message := "Debugging in Go"
     7:       fmt.Println(message)
     8: }
(dlv) print message
"Debugging in Go"
```

Med dessa kommandon kan du stega igenom ditt program, inspektera tillståndet efterhand för att förstå hur det beter sig och identifiera eventuella problem.

## Fördjupning
Valet av `delve` som Go:s föredragna felsökningsverktyg framför traditionella verktyg som GDB (GNU Debugger) beror framför allt på Go:s exekveringsmodell och körningstid. GDB designades inte från början med Go:s körningstid i åtanke, vilket gör `delve` till ett mer lämpligt val för Go-utvecklare. `Delve` är specifikt utformat för Go och erbjuder en mer intuitiv felsökningsupplevelse för Go-rutiner, kanaler och andra Go-specifika konstruktioner.

Dessutom stöder `delve` ett brett utbud av funktioner utöver de som erbjuds av grundläggande GDB när man arbetar med Go-program. Dessa inkluderar men är inte begränsade till: att koppla till körande processer för felsökning; villkorliga brytpunkter; och att utvärdera komplexa uttryck som kan involvera Go:s samtidighetsprimitiver.

Även om `delve` är det go-to felsökningsverktyget för många Go-utvecklare, är det värt att notera att Go-verktygskedjan också inkluderar lättviktsformer av felsökningsstöd, såsom det inbyggda verktyget `pprof` för profilering och verktyget `trace` för visualisering av samtidighet. Dessa verktyg kan ibland erbjuda en snabbare eller mer övergripande lösning för att diagnostisera prestandaproblem i program eller samtidighetsbuggar, vilka kan vara kompletterande eller till och med att föredra beroende på felsökningssammanhanget.
