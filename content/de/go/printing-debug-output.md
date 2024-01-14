---
title:                "Go: Druck von Fehlerausgaben"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Wenn man Software entwickelt, kommt man oft an den Punkt, an dem man das Verhalten des Programms verstehen muss. Hierfür ist das Ausgeben von Debug-Ausgaben ein unverzichtbares Werkzeug. Es ermöglicht uns, den Programmablauf zu verfolgen und mögliche Fehler zu finden.

## Wie geht man vor

Das Ausgeben von Debug-Ausgaben ist in Go mit der Funktion `fmt.Println()` sehr einfach. Diese Funktion gibt einen übergebenen String und die Werte von Variablen in der Konsole aus. Hier ein Beispiel:

```Go
// Variablen mit Werten belegen
name := "Franz"
age := 32

// Ausgabe mit fmt.Println()
fmt.Println("Name:", name)
fmt.Println("Alter:", age)
```

Die Ausgabe würde folgendermaßen aussehen:

```
Name: Franz
Alter: 32
```

Neben `fmt.Println()` gibt es auch noch andere Funktionen wie `fmt.Printf()` für formatierte Ausgaben oder `fmt.Errorf()` für Fehlermeldungen. Es lohnt sich, die Dokumentation von Go genauer zu studieren, um die passende Debugging-Funktion zu finden.

## Tiefergehende Informationen

Neben den Standardfunktionen bietet Go auch die Möglichkeit, eigene Funktionalitäten für das Debugging zu implementieren. Mit dem Paket `log` können eigene Logger erstellt werden, die genau angeben, wo die Ausgaben herkommen. Außerdem gibt es das Paket `debug/trace`, welches es ermöglicht, den Programmablauf schrittweise zu verfolgen.

## Siehe auch

- [Go Dokumentation zu Debugging](https://golang.org/pkg/debug/)
- [Einführung in das Debuggen mit Go](https://medium.com/@ashtaag/debugging-go-applications-92298601b28c)
- [Wie man effektiv Debug-Ausgaben nutzt](https://blog.gopheracademy.com/advent-2014/rewriting-log-functions/)