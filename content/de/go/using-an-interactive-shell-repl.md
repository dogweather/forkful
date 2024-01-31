---
title:                "Nutzung einer interaktiven Shell (REPL)"
date:                  2024-01-26T04:14:19.638631-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nutzung einer interaktiven Shell (REPL)"

category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein REPL (Read-Eval-Print Loop) ermöglicht die Live-Interaktion mit Code; es liest Eingaben, wertet sie aus, druckt das Ergebnis und beginnt von vorne. Programmierer nutzen es, um Code-Schnipsel zu testen, zu debuggen und in Echtzeit neue Sprachen zu lernen.

## Wie:
Go beinhaltet standardmäßig kein eingebautes REPL, aber Sie können Drittanbieter-Tools verwenden. Ein beliebtes Tool ist `gore`:

```go
// Installiere gore mit
$ go install github.com/motemen/gore/cmd/gore@latest

// Starte gore
$ gore
gore version 0.5.0  :help für Hilfe
gore> :import fmt
gore> fmt.Println("Hallo, Go REPL!")
Hallo, Go REPL!
nil
```

## Tiefergehende Betrachtung
Ursprünglich für Lisp entwickelt, sind REPLs häufig in dynamischen Sprachen wie Python oder Ruby zu finden. Go, als statisch typisierte Sprache, beinhaltet standardmäßig kein solches Tool. Alternativen zu `gore` umfassen `go-pry` und `yaegi`. Diese Tools interpretieren Go-Code und ermöglichen es Ihnen, Ideen schnell zu erkunden und zu validieren, ohne eine vollständige App zu kompilieren. Sie sind besonders nützlich für Anfänger und in Bildungskontexten, wo der Fokus auf Lernen und Experimentieren liegt.

## Siehe auch
- `gore`: [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry`: [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry) 
- `yaegi`: [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)
