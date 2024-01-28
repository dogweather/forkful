---
title:                "Einsatz eines Debuggers"
date:                  2024-01-26T03:50:03.877300-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einsatz eines Debuggers"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/using-a-debugger.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein Debugger zu verwenden ist, als hätte man ein GPS im Dschungel des Codes; er führt dich zur Quelle des Problems. Programmierer nutzen Debugger, um Schritt für Schritt durch ihren Code zu gehen, Variablen zu inspizieren und den Ablauf zu verstehen, was es einfacher macht, Fehler zu finden und die Leistung zu optimieren.

## Wie geht das:
Go hat ein eingebautes Werkzeug zum Debuggen namens Delve (`dlv`). Um zu starten, installiere Delve, schreibe ein einfaches Go-Programm und führe es dann durch den Debugger.

```Go
// Zuerst Delve installieren
// go get -u github.com/go-delve/delve/cmd/dlv

// Beispiel Go-Programm, speichere als main.go
package main

import "fmt"

func main() {
    message := "Debugging mit Delve!"
    fmt.Println(message)
}

// Führe dein Programm mit Delve aus
// dlv debug

// Einige grundlegende Delve-Befehle:
// (dlv) break main.main // einen Haltepunkt bei der Funktion main setzen
// (dlv) continue // Ausführen bis zum Haltepunkt oder Programmbeendigung 
// (dlv) step // schrittweise durch das Programm gehen
// (dlv) print message // den aktuellen Wert der Variablen 'message' ausgeben
// (dlv) quit // Delve beenden
```

`dlv debug` zu starten, beginnt eine Debugging-Sitzung. Sobald du einen Haltepunkt erreichst, den du gesetzt hast, kannst du Schritt für Schritt durch dein Programm gehen und sehen, was unter der Haube passiert.

## Tiefergehender Einblick
Historisch gesehen haben Go-Programmierer mehrere Werkzeuge zum Debuggen benutzt, wie z.B. GDB (GNU Debugger), stießen aber auf Herausforderungen, da GDB nicht für Gos Laufzeit und Goroutinen angepasst war. Delve kam zur Rettung mit besserer Unterstützung für Gos einzigartige Merkmale.

Es gibt Alternativen zu Delve wie `go-dbg`, und sogar integrierte Debugger-Unterstützung innerhalb von IDEs wie Visual Studio Code und GoLand, die Delve umgeben, um eine benutzerfreundlichere Erfahrung zu bieten.

Auf der Implementierungsseite arbeitet Delve mit den Paketen `runtime` und `debug/gosym` unter anderen, um auf Go-Programmsymbole und Laufzeitinformationen zuzugreifen und diese zu interpretieren. Es wird ständig aktualisiert, um mit neuen Sprachmerkmalen und -versionen Schritt zu halten.

## Siehe auch
- Das offizielle Repo von Delve: https://github.com/go-delve/delve
- Go-Debugger-Tutorial vom Go-Team: https://golang.org/doc/gdb
- Go-Debugging in Visual Studio Code: https://code.visualstudio.com/docs/languages/go#_debugging
