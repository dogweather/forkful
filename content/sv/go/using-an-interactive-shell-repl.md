---
title:                "Använda en interaktiv skal (REPL)"
date:                  2024-01-26T04:14:45.468997-07:00
model:                 gpt-4-0125-preview
simple_title:         "Använda en interaktiv skal (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Vad & Varför?
En REPL (Read-Eval-Print Loop) låter dig interagera med kod live; den läser inmatning, utvärderar den, skriver ut resultatet och loopar tillbaka. Programmerare använder den för att testa kodsnuttar, felsöka och lära sig nya språk i realtid.

## Hur man gör:
Go innehåller inte en inbyggd REPL, men du kan använda tredjepartsverktyg. Ett populärt verktyg är `gore`:

```go
// Installera gore med
$ go install github.com/motemen/gore/cmd/gore@latest

// Kör gore
$ gore
gore version 0.5.0  :help för hjälp
gore> :import fmt
gore> fmt.Println("Hej, Go REPL!")
Hej, Go REPL!
nil
```

## Djupdykning
Ursprungligen utvecklad för Lisp, är REPLs vanliga i dynamiska språk som Python eller Ruby. Go, som är statiskt typat, inkluderar inte en sådan direkt från början. Alternativ till `gore` inkluderar `go-pry` och `yaegi`. Dessa verktyg tolkar Go-kod, vilket låter dig utforska och validera idéer snabbt utan att kompilera en fullfjädrad app. De är särskilt användbara för nybörjare och i utbildningssammanhang där fokus ligger på lärande och experiment.

## Se även
- `gore`: [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry`: [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry) 
- `yaegi`: [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)
