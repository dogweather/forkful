---
title:                "Go: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich überhaupt mit der Erstellung von temporären Dateien beschäftigen? Nun, temporäre Dateien sind nützlich, wenn man Daten vorübergehend speichern muss, um sie später zu verarbeiten oder sie zu einem späteren Zeitpunkt wieder zu löschen. Zum Beispiel können sie in der Zwischenablage oder als Zwischenspeicher beim Herunterladen von Dateien verwendet werden.

## Wie
Das Erstellen einer temporären Datei in Go ist eigentlich recht einfach. Zunächst müssen wir das `os` Paket importieren, damit wir auf die Funktionen zum Erstellen von Dateien zugreifen können.

```
import (
    "fmt"
    "io/ioutil"
    "os"
)
```

Als nächstes erstellen wir eine temporäre Datei mit dem Prefix "example" und der Erweiterung ".txt". Das `ioutil.TempFile` Funktion gibt ein `os.File` Objekt zurück.

```
file, err := ioutil.TempFile("", "example*.txt")
if err != nil {
    panic(err)
}
```

Wir können dann den Dateinamen oder den Pfad der temporären Datei mit Hilfe von `file.Name()` abrufen. Wir können auch Schreib- oder Leserechte auf die Datei setzen oder auf sie schreiben.

```
fmt.Println("Temporäre Datei erstellt:", file.Name())
file.WriteString("Hallo, Welt!")
file.Sync()
```

Um die temporäre Datei am Ende zu löschen, können wir `os.Remove` Funktion verwenden.

```
defer os.Remove(file.Name())
```

## Deep Dive
Beim Erstellen einer temporären Datei gibt es einige Dinge zu beachten. Zum Beispiel kann es sein, dass die Datei beim Erstellen bereits existiert oder dass die Datei erst später gelöscht wird. Um diese potenziellen Probleme zu vermeiden, können wir das `ioutil.TempDir` Funktion verwenden, um einen temporären Ordner zu erstellen, in dem wir unsere temporäre Datei platzieren können. Wir können auch die Prefix- und Suffixparameter verwenden, um sicherzustellen, dass unsere temporäre Datei einen eindeutigen Namen hat.

```
tempDir, err := ioutil.TempDir("", "example")
if err != nil {
    panic(err)
}
defer os.Remove(tempDir)
file, err := ioutil.TempFile(tempDir, "tempfile*.txt")
if err != nil {
    panic(err)
}
```

## Siehe auch
- [Das `os` Paket in der offiziellen Dokumentation von Go](https://golang.org/pkg/os/)
- [Temporäre Dateien in Go von Gopher Academy](https://blog.gopheracademy.com/advent-2015/safely-creating-temporary-files/)
- [Ein Stack Overflow Eintrag über die Verwendung von `ioutil.TempFile`](https://stackoverflow.com/questions/22424365/how-to-create-a-temporary-file-in-go)