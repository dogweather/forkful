---
title:                "Het gebruik van een interactieve shell (REPL)"
date:                  2024-01-28T22:09:03.753588-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het gebruik van een interactieve shell (REPL)"

category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een REPL (Read-Eval-Print Loop) stelt je in staat om live met code te interageren; het leest invoer, evalueert deze, print het resultaat en begint opnieuw. Programmeurs gebruiken het om snippets te testen, te debuggen en in real-time nieuwe talen te leren.

## Hoe te gebruiken:
Go bevat geen ingebouwde REPL, maar je kunt wel gebruikmaken van tools van derden. Een populaire tool is `gore`:

```go
// Installeer gore met
$ go install github.com/motemen/gore/cmd/gore@latest

// Start gore
$ gore
gore versie 0.5.0  :help voor hulp
gore> :import fmt
gore> fmt.Println("Hello, Go REPL!")
Hello, Go REPL!
nil
```

## Diepere Duik
Oorspronkelijk ontwikkeld voor Lisp, zijn REPL's gangbaar in dynamische talen zoals Python of Ruby. Go, zijnde statisch getypt, bevat er niet standaard een. Alternatieven voor `gore` zijn onder andere `go-pry` en `yaegi`. Deze tools interpreteren Go code, waardoor je snel ideeën kunt verkennen en valideren zonder een volledige app te compileren. Ze zijn vooral nuttig voor beginners en in educatieve contexten waar de focus ligt op leren en experimenteren.

## Zie Ook
- `gore`: [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry`: [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry)
- `yaegi`: [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)
