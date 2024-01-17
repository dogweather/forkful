---
title:                "Ein Textdokument lesen"
html_title:           "Go: Ein Textdokument lesen"
simple_title:         "Ein Textdokument lesen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen einer Textdatei bedeutet, dass ein Programmierer den Inhalt einer Textdatei auf dem Computer ausliest und auf die darin enthaltenen Informationen zugreifen kann. Programmierer tun dies, um zum Beispiel Daten aus einer externen Datei in ihr Programm zu importieren oder um den Inhalt einer Datei zu analysieren.

## Wie geht's?

Öffne eine Textdatei in Go ist ganz einfach. Hier ist ein Beispielcode, um eine Textdatei namens "beispiel.txt" zu lesen:

```Go
datei, fehler := os.Open("beispiel.txt")
wenn fehler != nil {
    fmt.Println(fehler)
}

scanner := bufio.NewScanner(datei)
für scanner.Scan() {
    fmt.Println(scanner.Text())
}

datei.Schließen()
```

Die erste Zeile öffnet die Datei "beispiel.txt" und speichert sie in der Variablen "datei". Wenn es einen Fehler beim Öffnen der Datei gibt, wird dieser in der Variable "fehler" gespeichert und ausgegeben.

Die nächsten Zeilen erstellen einen Scanner, der die Datei Zeile für Zeile durchgeht und den Inhalt ausgibt. Zum Schluss wird die Datei geschlossen, um sicherzustellen, dass keine Ressourcen verschwendet werden.

## Tiefere Einblicke

Das Lesen von Textdateien gehört zu den grundlegenden Funktionen jeder Programmiersprache. Es ist eine effiziente Möglichkeit, Daten aus externen Quellen zu importieren und zu verarbeiten.

Es gibt auch alternative Methoden, um Textdateien in Go zu lesen, wie beispielsweise die `ioutil.ReadFile` Funktion. Diese liest den gesamten Inhalt einer Datei auf einmal und gibt ihn als Byte-Array zurück.

Bei der Implementierung des Dateilesens sollten Programmierer auch auf die Wahl des Dateiformats achten. Zum Beispiel kann es bestimmte Funktionen geben, die nur mit CSV-Dateien funktionieren, während andere besser für JSON-Dateien geeignet sind.

## Siehe auch

- Offizielle Dokumentation von Go zur Textverarbeitung: https://golang.org/pkg/text
- Tutorial zur Verwendung der `os` Bibliothek in Go: https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-in-go
- Artikel über die Verwendung von `ioutil` in Go: https://golangbot.com/read-files/