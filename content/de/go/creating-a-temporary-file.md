---
title:                "Go: Erstellen einer temporären Datei"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Oftmals ist es notwendig, temporäre Dateien in Programmen zu erstellen. Diese dienen meistens dazu, Daten für eine kurze Zeit zu speichern, bevor sie wieder gelöscht werden. In Go gibt es verschiedene Möglichkeiten, eine temporäre Datei zu erstellen. Im folgenden Artikel zeigen wir Ihnen, wie Sie das am besten in Ihrer Go-Anwendung umsetzen können.

## Wie erstelle ich eine temporäre Datei in Go

Die Erstellung einer temporären Datei in Go ist sehr einfach und kann auf verschiedene Weisen erfolgen. Eine Möglichkeit ist die Verwendung der `ioutil`-Bibliothek zusammen mit der `TempFile`-Funktion. Diese akzeptiert zwei Parameter: ein Verzeichnis und ein optionales Präfix für die Dateinamen. Um eine temporäre Datei zu erstellen, verwenden wir folgenden Code:

```Go
tempFile, err := ioutil.TempFile("/tmp", "example")
if err != nil {
    panic(err)
}
defer os.Remove(tempFile.Name())
``` 

Wir verwenden hier `/tmp` als Verzeichnis und `example` als Präfix. Sie können diese Werte aber auch an Ihre eigenen Bedürfnisse anpassen. Die erstellte Datei wird automatisch im angegebenen Verzeichnis gespeichert und der Name wird in der Konsole ausgegeben. Außerdem wurde in diesem Beispiel auch der `defer` Befehl verwendet, um die Datei automatisch zu löschen, sobald das Programm beendet wird.

## Tiefergehende Informationen zur Erstellung von temporären Dateien

Die `TempFile`-Funktion verwendet unter der Haube die `mkstemp`-Funktion des Betriebssystems, um eine zufällige, eindeutige Datei zu erstellen. Diese wird geöffnet und kann anschließend genutzt werden. Das bedeutet, dass Sie selbst keine eindeutigen Dateinamen generieren müssen, was das Risiko von Kollisionen beim benutzen von Präfixen oder Zufallszahlen minimiert.

Eine weitere Möglichkeit ist die Nutzung der `TempDir`-Funktion, um ein temporäres Verzeichnis zu erstellen. Der Ablauf ist ähnlich wie bei `TempFile`, nur dass hier ein Verzeichnis statt einer Datei erstellt wird. Diese Funktion kann nützlich sein, wenn Sie mehrere Dateien temporär speichern müssen.

## Siehe auch

- [Die vollständige Dokumentation zur `ioutil`-Bibliothek](https://golang.org/pkg/io/ioutil/)
- [Weitere Informationen zur `mkstemp`-Funktion](https://www.man7.org/linux/man-pages/man3/mkstemp.3p.html)
- [Ein Tutorial zur Verwendung von temporären Dateien in Go](https://golangdocs.com/create-temporary-files-in-go)