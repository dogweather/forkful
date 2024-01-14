---
title:    "Go: Fehlerbehebungsausgabe drucken"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Beim Programmieren ist es oft hilfreich, den aktuellen Zustand des Codes zu überprüfen und gezielt Fehler zu suchen. Hierbei kann das Ausdrucken von Debug-Ausgaben eine nützliche Methode sein.

## Wie geht das?

Um Debug-Ausgaben in Go zu drucken, kann die Funktion "fmt.Println()" verwendet werden. Hierbei kann sowohl Text als auch Variablen übergeben werden, die dann in der Ausgabe erscheinen.

```Go
name := "Sandra"
age := 25
fmt.Println("Der Name ist", name, "und das Alter ist", age)
```

Diese Funktion gibt folgende Ausgabe aus:

```
Der Name ist Sandra und das Alter ist 25
```

Es ist auch möglich, Variablenwerte in einem bestimmten Format zu drucken, zum Beispiel als Dezimalzahl oder in hexadezimaler Darstellung. Hierfür gibt es die Funktion "fmt.Printf()", bei der zusätzlich ein Formatierungsstring angegeben werden muss.

```Go
favoriteNumber := 8
fmt.Printf("Meine Lieblingszahl ist %d und es ist die %xte im Alphabet", favoriteNumber, favoriteNumber)
```

Die Ausgabe sieht wie folgt aus:

```
Meine Lieblingszahl ist 8 und es ist die 8te im Alphabet
```

## Tieferer Einblick

Es gibt verschiedene Möglichkeiten, Debug-Ausgaben in Go zu nutzen. So kann man zum Beispiel mit der Funktion "log.Println()" Ausgaben in einer Log-Datei schreiben, statt sie direkt in der Konsole auszugeben. Außerdem gibt es noch weitere Formatierungsverfahren für die Funktionen "fmt.Printf()" und "log.Println()", die in der offiziellen Go-Dokumentation genauer erläutert werden.

## Siehe auch

- Offizielle Go-Dokumentation: https://golang.org/doc/
- Weitere Informationen zu Debugging in Go: https://golang.org/doc/gdb
- Blogartikel zu Debugging-Techniken in Go: https://blog.golang.org/debugging-techniques