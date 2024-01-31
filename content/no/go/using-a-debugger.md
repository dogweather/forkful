---
title:                "Å bruke en feilsøker"
date:                  2024-01-26T03:50:11.345297-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å bruke en feilsøker"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/using-a-debugger.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Å bruke en debugger er som å ha en GPS i kodejungelen; den leder deg til problemets kilde. Programmerere bruker debuggerer til å steg-for-steg gå gjennom koden sin, inspisere variabler og forstå flyten, noe som gjør det enklere å fange opp feil og optimalisere ytelsen.

## Hvordan:
Go har et innebygd verktøy for feilsøking kalt Delve (`dlv`). For å komme i gang, installer Delve, skriv et enkelt Go-program, og kjør det deretter gjennom debuggeren.

```Go
// Først, installer Delve
// go get -u github.com/go-delve/delve/cmd/dlv

// Eksempel Go-program, lagre som main.go
pakke main

importer "fmt"

funksjon main() {
    melding := "Feilsøking med Delve!"
    fmt.Println(melding)
}

// Kjør programmet ditt med Delve
// dlv feilsøk

// Noen grunnleggende Delve-kommandoer:
// (dlv) bryt main.main // sett et brytepunkt ved funksjon main
// (dlv) fortsett // kjør til brytepunkt eller programavslutning
// (dlv) steg // enkeltsteg gjennom programmet
// (dlv) skriv ut melding // skriv ut gjeldende verdi av variabelen 'melding'
// (dlv) avslutt // avslutt Delve
```

Å kjøre `dlv debug` starter en feilsøkingssesjon. Når du treffer et brytepunkt du har satt, kan du steg-for-steg gå gjennom programmet ditt og se hva som foregår under panseret.

## Dypdykk
Historisk sett har Go-programmerere brukt flere verktøy for feilsøking som GDB (GNU Debugger), men møtt utfordringer fordi GDB ikke var tilpasset Go sitt kjøretidssystem og gorutiner. Delve kom til unnsetning med bedre støtte for Gos unike funksjoner.

Det finnes alternativer til Delve som `go-dbg`, og til og med integrert feilsøkingsstøtte innenfor IDEer som Visual Studio Code og GoLand, som omslutter Delve for en mer brukervennlig opplevelse.

På implementeringssiden fungerer Delve ved å bruke `runtime` og `debug/gosym`-pakkene, blant andre, for å få tilgang til og tolke Go-programsymboler og kjøretidsinformasjon. Det oppdateres kontinuerlig for å holde tritt med nye språkfunksjoner og -versjoner.

## Se også
- Delves offisielle repo: https://github.com/go-delve/delve
- Go Debugger-opplæring av Go-laget: https://golang.org/doc/gdb
- Feilsøking i Visual Studio Code Go: https://code.visualstudio.com/docs/languages/go#_debugging
