---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:32.601714-07:00
description: "Wie geht das: In Go wird die String-Interpolation \xFCblicherweise mit\
  \ dem `fmt`-Paket erreicht, insbesondere mit der Funktion `Sprintf`, die es Ihnen\u2026"
lastmod: '2024-03-13T22:44:53.272088-06:00'
model: gpt-4-0125-preview
summary: "In Go wird die String-Interpolation \xFCblicherweise mit dem `fmt`-Paket\
  \ erreicht, insbesondere mit der Funktion `Sprintf`, die es Ihnen erm\xF6glicht,\
  \ Variablen in einen String einzuf\xFCgen, indem Formatierungsverben angegeben werden."
title: Interpolation eines Strings
weight: 8
---

## Wie geht das:
In Go wird die String-Interpolation üblicherweise mit dem `fmt`-Paket erreicht, insbesondere mit der Funktion `Sprintf`, die es Ihnen ermöglicht, Variablen in einen String einzufügen, indem Formatierungsverben angegeben werden. Die Verben sind Platzhalter im Formatierungsstring und werden durch die Werte der gegebenen Variablen ersetzt. So verwenden Sie es:

```go
package main

import (
    "fmt"
)

func main() {
    name := "Jane"
    age := 28

    // Verwendung von Sprintf für die String-Interpolation
    message := fmt.Sprintf("Hallo, mein Name ist %s und ich bin %d Jahre alt.", name, age)
    fmt.Println(message) // Ausgabe: Hallo, mein Name ist Jane und ich bin 28 Jahre alt.
}
```

Beachten Sie, dass `%s` für Strings und `%d` für Ganzzahlen verwendet wird. Die Dokumentation des `fmt`-Pakets bietet eine umfassende Liste von Formatierungsverben für verschiedene Datentypen.

## Tiefere Einblicke
Das Konzept der String-Interpolation existiert in vielen Programmiersprachen, allerdings mit unterschiedlichen Syntaxen und Fähigkeiten. In Go, obwohl die Funktion `Sprintf` des `fmt`-Pakets der am häufigsten verwendete Ansatz ist, ist es möglicherweise nicht immer der effizienteste, besonders für einfache Verkettungen oder wenn in hochleistungssensitivem Code gearbeitet wird.

Das `fmt`-Paket verwendet Reflexion, um die Typen der Variablen zur Laufzeit dynamisch zu interpretieren, was, obwohl flexibel, Overhead verursacht. Für Szenarien, in denen Leistung kritisch ist, können direkte String-Verkettung oder der Typ `strings.Builder` bessere Alternativen bieten. Direkte Verkettung ist unkompliziert, kann aber bei mehreren Variablen unhandlich werden. `strings.Builder` bietet andererseits eine leistungsstärkere und lesbarere Möglichkeit, komplexe Strings in einer Schleife oder beim Umgang mit vielen Variablen zu erstellen:

```go
var sb strings.Builder
sb.WriteString("Hallo, mein Name ist ")
sb.WriteString(name)
sb.WriteString(" und ich bin ")
sb.WriteString(strconv.Itoa(age))
sb.WriteString(" Jahre alt.")
message := sb.String()

fmt.Println(message) // Gibt dasselbe aus wie zuvor
```

Letztendlich hängt die Wahl zwischen `fmt.Sprintf`, direkter Verkettung und `strings.Builder` von den spezifischen Anforderungen Ihrer Anwendung ab, wie der Komplexität des zu konstruierenden Strings und Leistungserwägungen.
