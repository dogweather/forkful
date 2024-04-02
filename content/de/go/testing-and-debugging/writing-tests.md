---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:46.089911-07:00
description: "Tests in Go zu schreiben, bedeutet, kleine, handhabbare Code-Schnipsel\
  \ zu erstellen, die die Funktionalit\xE4t und das Verhalten Ihrer Anwendung validieren.\u2026"
lastmod: '2024-03-13T22:44:53.291491-06:00'
model: gpt-4-0125-preview
summary: "Tests in Go zu schreiben, bedeutet, kleine, handhabbare Code-Schnipsel zu\
  \ erstellen, die die Funktionalit\xE4t und das Verhalten Ihrer Anwendung validieren.\u2026"
title: Tests schreiben
weight: 36
---

## Was & Warum?

Tests in Go zu schreiben, bedeutet, kleine, handhabbare Code-Schnipsel zu erstellen, die die Funktionalität und das Verhalten Ihrer Anwendung validieren. Programmierer schreiben Tests, um sicherzustellen, dass ihr Code unter verschiedenen Bedingungen wie erwartet funktioniert, um Refactoring zu erleichtern und um Regressionen zu verhindern.

## Wie:

In Go werden Tests üblicherweise im selben Paket wie der zu testende Code geschrieben. Dateien, die Tests enthalten, werden mit dem Suffix `_test.go` benannt. Tests sind Funktionen, die einen Zeiger auf das testing.T-Objekt (aus dem `testing` Paket) als Argument nehmen, und sie signalisieren einen Fehler, indem sie Methoden wie `t.Fail()`, `t.Errorf()` usw. aufrufen.

Beispiel eines einfachen Tests für eine Funktion `Add`, definiert in `math.go`:
```go
// math.go
package math

func Add(x, y int) int {
    return x + y
}
```

Testdatei `math_test.go`:
```go
package math

import "testing"

func TestAdd(t *testing.T) {
    result := Add(1, 2)
    expected := 3
    if result != expected {
        t.Errorf("Add(1, 2) = %d; want %d", result, expected)
    }
}
```

Führen Sie Ihre Tests mit dem Befehl `go test` im selben Verzeichnis wie Ihre Testdateien aus. Eine beispielhafte Ausgabe, die einen bestandenen Test anzeigt, könnte so aussehen:

```
PASS
ok      example.com/my/math 0.002s
```

Für tabelle-getriebene Tests, die es Ihnen ermöglichen, effizient verschiedene Eingabe- und Ausgabe-Kombinationen zu testen, definieren Sie ein Slice von Structs, die Testfälle repräsentieren:

```go
func TestAddTableDriven(t *testing.T) {
    var tests = []struct {
        x        int
        y        int
        expected int
    }{
        {1, 2, 3},
        {2, 3, 5},
        {-1, -2, -3},
    }

    for _, tt := range tests {
        testname := fmt.Sprintf("%d+%d", tt.x, tt.y)
        t.Run(testname, func(t *testing.T) {
            ans := Add(tt.x, tt.y)
            if ans != tt.expected {
                t.Errorf("got %d, want %d", ans, tt.expected)
            }
        })
    }
}
```

## Tiefere Einblicke

Das Go-Test-Framework, eingeführt in Go 1 zusammen mit der Sprache selbst, wurde entworfen, um nahtlos in die Go-Toolchain zu integrieren und spiegelt die Betonung von Go auf Einfachheit und Effizienz in der Softwareentwicklung wider. Anders als einige Test-Frameworks in anderen Sprachen, die sich auf externe Bibliotheken oder komplexe Setups verlassen, bietet das eingebaute `testing` Paket von Go eine unkomplizierte Weise, Tests zu schreiben und auszuführen.

Ein interessanter Aspekt von Gos Ansatz zum Testen ist das Prinzip der Konvention über Konfiguration, das es annimmt, wie das Dateinamensmuster (`_test.go`) und die Nutzung von Standardbibliotheksfunktionalitäten über externe Abhängigkeiten. Dieser minimalistische Ansatz ermutigt Entwickler, Tests zu schreiben, da die Einstiegshürde niedrig ist.

Obwohl die eingebauten Testeinrichtungen von Go viel abdecken, gibt es Szenarien, in denen Drittanbieter-Tools oder Frameworks mehr Funktionalitäten bieten, wie Mock-Generierung, Fuzz-Testing oder Test im Stil der behaviour-driven development (BDD). Beliebte Bibliotheken wie Testify oder GoMock ergänzen Gos standardmäßige Testfähigkeiten und bieten ausdrucksstärkere Behauptungen oder Mock-Generierungsfähigkeiten, die besonders nützlich in komplexen Anwendungen mit vielen Abhängigkeiten sein können.

Trotz der Existenz dieser Alternativen bleibt das Standard-Go-Testpaket der Eckpfeiler für das Testen in Go aufgrund seiner Einfachheit, Leistung und engen Integration mit der Sprache und der Toolchain. Ob Entwickler es mit Drittanbieter-Tools ergänzen oder nicht, das Go-Test-Framework bietet eine solide Grundlage, um die Codequalität und Zuverlässigkeit zu gewährleisten.
