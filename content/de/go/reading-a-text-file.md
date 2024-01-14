---
title:                "Go: Eine Textdatei lesen"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Warum

Das Lesen von Textdateien ist ein grundlegender und wichtiger Prozess in der Softwareentwicklung. Es ermöglicht uns, Daten zu lesen, zu verarbeiten und zu analysieren, die in einfacher Textform vorliegen. In dieser Blog-Post werden wir uns ansehen, wie wir Textdateien in Go programmieren können.

# Wie geht man vor

Um eine Textdatei in Go zu lesen, müssen wir zuerst die `os` Bibliothek importieren. Diese ermöglicht es uns, auf Dateien und Ordner im Betriebssystem zuzugreifen. Dann können wir eine `File` Variable erstellen, die die zu lesende Textdatei repräsentiert.

```Go
import "os"

file, err := os.Open("example.txt")
```

Wir überprüfen auch, ob beim Öffnen der Datei ein Fehler aufgetreten ist. Falls nicht, können wir die Daten aus der Textdatei lesen und sie in eine Variable speichern.

```Go
if err == nil {
    data := make([]byte, 1024)
    count, err := file.Read(data)
}
```

Die `Read` Funktion liest die angegebene Anzahl von Bytes aus der Datei und speichert sie im angegebenen `[]byte` Slice. Wenn wir die gesamte Datei lesen möchten, müssen wir eine ausreichend große Länge für den Slice angeben. Schließlich können wir den Inhalt der Datei ausgeben und die Datei schließen.

```Go
fmt.Println(string(data[:count]))
file.Close()
```

# Tiefer Einblick

Das Lesen von Textdateien kann auch mit Hilfe der `bufio` Bibliothek erfolgen. Diese bietet einige zusätzliche Funktionen wie zum Beispiel das Lesen von Zeilen oder das Verarbeiten von Dateien, die größer als der verfügbare Arbeitsspeicher sind.

```Go
import (
    "bufio"
    "fmt"
    "os"
)

file, err := os.Open("example.txt")
if err != nil {
    panic(err)
}
defer file.Close()

scanner := bufio.NewScanner(file)
for scanner.Scan() {
    fmt.Println(scanner.Text())
}

if err := scanner.Err(); err != nil {
    panic(err)
}
```

In diesem Beispiel nutzen wir den `Scanner` um die Textdatei zeilenweise zu lesen und jede Zeile direkt auszugeben. Dies ist besonders praktisch, wenn wir mit großen Dateien arbeiten, da nur eine Zeile auf einmal im Arbeitsspeicher gehalten werden muss. Auch hier müssen wir darauf achten, die Datei nach der Verwendung zu schließen.

# Siehe auch

- [Die `os` Bibliothek in der offiziellen Dokumentation von Go](https://golang.org/pkg/os/)
- [Die `bufio` Bibliothek in der offiziellen Dokumentation von Go](https://golang.org/pkg/bufio/)
- [Weitere Beispiele für das Lesen von Textdateien in Go](https://www.digitalocean.com/community/tutorials/how-to-read-and-write-files-in-go)