---
title:                "Verwenden eines Debuggers"
aliases: - /de/go/using-a-debugger.md
date:                  2024-02-03T18:10:16.367454-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verwenden eines Debuggers"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/using-a-debugger.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Die Verwendung eines Debuggers in der Go-Programmierung beinhaltet den Einsatz von Tools oder Funktionen, um den Zustand eines laufenden Programms zu inspizieren und zu modifizieren, um sein Verhalten zu verstehen oder Probleme zu diagnostizieren. Programmierer tun dies, um effizient Bugs zu finden und zu beheben, die Leistung zu optimieren und die Richtigkeit ihres Codes zu gewährleisten.

## Wie:

Go bietet eine integrierte Einrichtung für das Debugging namens `delve`. Es ist ein voll ausgestattetes Debugging-Tool, das es Ihnen ermöglicht, Go-Programme Schritt für Schritt auszuführen, Programmvariablen zu inspizieren und Ausdrücke zu bewerten.

Zu Beginn müssen Sie zunächst `delve` installieren. Dies können Sie tun, indem Sie folgendes ausführen:

```shell
go get -u github.com/go-delve/delve/cmd/dlv
```

Nun, debuggen wir ein einfaches Go-Programm. Betrachten Sie ein Programm `main.go`:

```go
package main

import "fmt"

func main() {
    message := "Debugging in Go"
    fmt.Println(message)
}
```

Um mit dem Debuggen dieses Programms zu beginnen, öffnen Sie ein Terminal im Verzeichnis des Projekts und führen Sie aus:

```shell
dlv debug
```

Dieser Befehl kompiliert das Programm mit deaktivierten Optimierungen (um das Debugging-Erlebnis zu verbessern), startet es und hängt einen Debugger an.

Sobald `delve` läuft, befinden Sie sich in der interaktiven Debugger-Shell. Hier sind einige grundlegende Befehle:

- `break main.main` setzt einen Haltepunkt an die Funktion `main`.
- `continue` setzt die Programmausführung fort, bis ein Haltepunkt erreicht wird.
- `print message` druckt den Wert der Variablen `message`.
- `next` führt die Programmausführung zur nächsten Zeile fort.
- `quit` verlässt den Debugger.

Die Ausgabe beim Erreichen des Haltepunktes und beim Ausdrucken der Variablen könnte folgendermaßen aussehen:

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

Mit diesen Befehlen können Sie durch Ihr Programm schreiten, den Zustand inspizieren und verstehen, wie es sich verhält, und Probleme identifizieren.

## Tiefere Einblicke

Die Wahl von `delve` als Go's bevorzugtem Debugging-Tool gegenüber traditionellen Tools wie GDB (GNU Debugger) liegt hauptsächlich in der Natur des Ausführungsmodells und der Laufzeitumgebung von Go. GDB wurde ursprünglich nicht mit der Go-Laufzeit im Hinterkopf entworfen, was `delve` zu einer geeigneteren Wahl für Go-Entwickler macht. `Delve` ist speziell für Go konzipiert und bietet ein intuitiveres Debugging-Erlebnis für Go-Routinen, Kanäle und andere spezifische Go-Konstrukte.

Darüber hinaus unterstützt `delve` eine breite Palette von Funktionen, die über die von GDB angebotenen Grundfunktionen für Go-Programme hinausgehen. Dazu gehören unter anderem: das Anhängen an laufende Prozesse zum Debuggen; bedingte Haltepunkte; und das Bewerten komplexer Ausdrücke, die Go's Parallelitätsprimitiven einschließen können.

Während `delve` für viele Go-Entwickler das bevorzugte Debugging-Tool ist, ist es erwähnenswert, dass die Go-Toolchain auch leichtgewichtigere Formen der Debugging-Unterstützung umfasst, wie das integrierte Tool `pprof` für das Profiling und das Tool `trace` für die Visualisierung von Nebenläufigkeit. Diese Tools können manchmal einen schnelleren oder höheren Ansatz für die Diagnose von Leistungsproblemen oder Nebenläufigkeits-Bugs bieten, die je nach Debugging-Kontext ergänzend oder sogar vorzuziehen sein könnten.
