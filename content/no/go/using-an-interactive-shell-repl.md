---
title:                "Bruke et interaktivt skall (REPL)"
date:                  2024-01-26T04:14:36.471302-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke et interaktivt skall (REPL)"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
En REPL (Read-Eval-Print Loop) lar deg samhandle med koden live; den leser input, evaluerer det, skriver ut resultatet, og looper tilbake. Programmerere bruker den til å teste utdrag, feilsøke og lære nye språk i sanntid.

## Hvordan:
Go inkluderer ikke en innebygd REPL, men du kan bruke tredjepartsverktøy. Et populært verktøy er `gore`:

```go
// Installer gore ved å bruke
$ go install github.com/motemen/gore/cmd/gore@latest

// Kjør gore
$ gore
gore versjon 0.5.0  :help for hjelp
gore> :import fmt
gore> fmt.Println("Hallo, Go REPL!")
Hallo, Go REPL!
nil
```

## Dypdykk
Opprinnelig utviklet for Lisp, er REPLer vanlige i dynamiske språk som Python eller Ruby. Go, som er statisk typet, inkluderer ikke en ut-av-boksen. Alternativer til `gore` inkluderer `go-pry` og `yaegi`. Disse verktøyene tolker Go-kode, og lar deg utforske og validere ideer raskt uten å kompilere en fullskala app. De er spesielt nyttige for nybegynnere og i utdanningskontekster der fokuset er på læring og eksperimentering.

## Se også
- `gore`: [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry`: [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry) 
- `yaegi`: [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)
