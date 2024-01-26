---
title:                "Eine Textdatei schreiben"
html_title:           "Arduino: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben einer Textdatei bedeutet, Textinformationen in einer Datei auf einem Speichermedium wie einer Festplatte zu speichern. Programmierer tun dies, um Daten dauerhaft zu sichern, Einstellungen zu speichern oder Informationen zwischen verschiedenen Teilen einer Anwendung oder verschiedenen Anwendungen auszutauschen.

## Wie geht das:
```Go
package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
    dateiname := "beispiel.txt"
    text := "Hallo, das ist ein Textbeispiel!"

    datei, err := os.Create(dateiname)
    if err != nil {
        fmt.Println("Fehler:", err)
        return
    }
    defer datei.Close()

    writer := bufio.NewWriter(datei)
    _, err = writer.WriteString(text + "\n")
    if err != nil {
        fmt.Println("Fehler:", err)
        return
    }
    err = writer.Flush()
    if err != nil {
        fmt.Println("Fehler:", err)
    }
}
```
Ausgabe: Der Text "Hallo, das ist ein Textbeispiel!" wird in die Datei `beispiel.txt` geschrieben.

## Vertiefung
Das Schreiben von Dateien ist so alt wie die Programmierung selbst. Ursprünglich wurden Textdateien für die Konfiguration von Software oder das Logging verwendet. Heutzutage gibt es Alternativen wie Datenbanken oder Cloud-Speicher, jedoch sind Textdateien wegen ihrer Einfachheit und Portabilität immer noch weit verbreitet. Bei der Implementierung in Go ist es wichtig, `defer` zu nutzen, um Dateien ordnungsgemäß zu schließen und Ressourcen freizugeben und `bufio` für effizientes Schreiben großer Mengen von Text zu verwenden.

## Siehe auch

- Go by Example: Dateioperationen: https://gobyexample.com/writing-files
- Go Dokumentation zum `os`-Paket: https://pkg.go.dev/os
- Go Dokumentation zum `bufio`-Paket: https://pkg.go.dev/bufio
