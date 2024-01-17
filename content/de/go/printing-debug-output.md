---
title:                "Debug-Ausgabe drucken"
html_title:           "Go: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/printing-debug-output.md"
---

{{< edit_this_page >}}

Was und warum?
Debug-Ausgaben sind eine Methode, um beim Programmieren hilfreiche Informationen auszugeben. Sie dienen dazu, Code während der Ausführung zu überwachen und Fehler zu finden. 

Wie geht's?
Sie können einfach `fmt.Println()` verwenden, um eine Debug-Ausgabe in Go zu erstellen. Hier ist ein Beispiel:
```Go
zahl := 10
fmt.Println("Die Variable zahl hat den Wert: ", zahl)
```
Dieser Code gibt die Zeile "Die Variable zahl hat den Wert: 10" aus. 

Sie können auch Variablenwerte und andere Informationen in der Debug-Ausgabe formatieren. Eine Möglichkeit ist die Verwendung von Platzhaltern mit `Printf()`:
```Go
name := "Lisa"
alter := 25
fmt.Printf("Mein Name ist %s und ich bin %d Jahre alt.", name, alter)
```
Dieser Code gibt die Zeile "Mein Name ist Lisa und ich bin 25 Jahre alt." aus.

Tief ins Detail:
Debug-Ausgaben gibt es schon seit den Anfängen der Programmierung und sind immer noch eine hilfreiche Methode, um Fehler zu finden und zu beheben. Es gibt auch andere Möglichkeiten, um Debugging durchzuführen, wie zum Beispiel das Verwenden von Breakpoints oder Debugging-Tools.

Die `fmt` Bibliothek in Go bietet verschiedene Funktionen zum Erstellen von Debug-Ausgaben, einschließlich `Print()`, `Println()` und `Printf()`. Sie können auch eigene Funktionen erstellen, die Debug-Ausgaben spezifisch für Ihr Projekt erstellen.

Weitere Informationen:
-Offizielle Dokumentation zu Debug-Ausgaben in Go: https://golang.org/pkg/fmt/
-Ein hilfreiches Tutorial zu Debugging in Go: https://www.callicoder.com/debugging-go-programs-with-vscode/
-Eine Übersicht über verschiedene Debugging-Tools in Go: https://blog.gopheracademy.com/advent-2019/some-golang-debugging-tools/