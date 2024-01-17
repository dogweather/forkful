---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "Go: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Aspekt der Programmierung, der häufig bei der Verwaltung von Dateien und Ordnern verwendet wird. Programmierer verwenden diese Funktion, um zu prüfen, ob ein bestimmtes Verzeichnis vorhanden ist, bevor sie darauf zugreifen oder damit arbeiten. Dadurch wird sichergestellt, dass der Code zuverlässig und robust ist.

## Wie geht's?

Um zu überprüfen, ob ein Verzeichnis mit Go existiert, müssen wir die Funktion "Exist" aus der Paketbibliothek "Os" importieren und die gewünschte Pfadangabe als Parameter übergeben. Hier ist ein Beispielcode:

```
package main
import "fmt"
import "os"

func main() {
    _, err := os.Stat("/pfad/zum/verzeichnis") 
    if os.IsNotExist(err) {
        fmt.Println("Verzeichnis existiert nicht") 
    } else {
        fmt.Println("Verzeichnis existiert") 
    } 
}
```

Die Ausgabe hängt davon ab, ob das Verzeichnis vorhanden ist oder nicht. Wenn das Verzeichnis nicht existiert, wird die erste Bedingung ausgeführt und die Meldung "Verzeichnis existiert nicht" ausgegeben.

## Tiefentauchen

Historisch gesehen gab es verschiedene Ansätze, um zu überprüfen, ob ein Verzeichnis existiert, mit verschiedenen Sprachen und Bibliotheken. Mit der Einführung von Go wurde diese Funktion in die Standardbibliothek aufgenommen, um die Verwaltung von Verzeichnissen zu vereinfachen.

Eine Alternative zur Verwendung der "Exist" Funktion besteht darin, das Verzeichnis zu öffnen und dann anhand von Fehlermeldungen festzustellen, ob es existiert oder nicht. Allerdings ist dies weniger effizient und kann zu unerwünschtem Verhalten führen.

Die "Exist" Funktion überprüft nicht nur, ob ein Verzeichnis existiert, sondern auch, ob auf das Verzeichnis zugegriffen werden kann. So können wir sicherstellen, dass das Verzeichnis nicht nur vorhanden, sondern auch zugänglich ist.

## Siehe auch

Weitere Informationen zum Überprüfen der Existenz von Verzeichnissen in Go finden Sie in der offiziellen Dokumentation: https://pkg.go.dev/os#File.Exist. Hier finden Sie auch Beispiele für die Verwendung der Funktion in verschiedenen Szenarien.