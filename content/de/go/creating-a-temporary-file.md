---
title:                "Eine temporäre Datei erstellen"
html_title:           "Go: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum

Bist du jemals in einer Situation gewesen, in der du ein temporäres Datei für deine Go-Anwendung erstellen musstest? Dieser Artikel erklärt dir, wie man ganz einfach eine temporäre Datei in Go erstellt und gibt auch noch einige zusätzliche Informationen für eine tiefgehende Eintauchen in dieses Thema.

# So geht's

Das Erstellen einer temporären Datei in Go erfordert nur wenige Zeilen Code. Im folgenden Beispiel wird die Funktion `TempFile()` aus dem Paket `ioutil` verwendet, um eine temporäre Datei mit dem Präfix "example" und der Dateiendung ".txt" zu erstellen.

```Go
file, err := ioutil.TempFile("", "example*.txt")
if err != nil {
  // handle error
}
defer os.Remove(file.Name()) // lösche die temporäre Datei am Ende
```

Die Funktion `TempFile()` akzeptiert zwei Parameter. Der erste ist die Pfad- oder Verzeichnis-Option, in der die temporäre Datei erstellt werden soll. Wenn dieser Option leer gelassen wird, wird die Standardpfad-Option verwendet. Der zweite Parameter ist das Präfix, dem der Name der temporären Datei vorangestellt wird. Dies ist optional, aber hilfreich, um später die Datei zu identifizieren.

In unserem Beispiel speichern wir die erstellte temporäre Datei in der Variable `file` und verwenden die `defer` Anweisung, um sicherzustellen, dass die Datei am Ende gelöscht wird. Dies ist wichtig, da temporäre Dateien nicht automatisch gelöscht werden und ansonsten unerwünschten Platz auf deinem System einnehmen können.

# Tief eintauchen

Wenn du eine detailliertere Kontrolle über die erstellte temporäre Datei benötigst, bietet das `ioutil` Paket noch zusätzliche Funktionen. Mit `TempDir()` kannst du eine temporäre Verzeichnis erstellen, und mit `ReadAll()` kannst du den Inhalt der temporären Datei auslesen.

Es gibt auch andere Pakete wie `os` und `io` welche ebenfalls Methoden zum Erstellen temporären Dateien beinhalten. Es ist wichtig, dass man die Vorteile und Unterschiede dieser verschiedenen Methoden versteht, bevor man sich für eine entscheidet.

# Siehe auch

- [ioutil Paket Dokumentation](https://golang.org/pkg/io/ioutil/)
- [os Paket Dokumentation](https://golang.org/pkg/os/)
- [Temporäre Dateien und Verzeichnisse in Go](https://www.alexandre-gomes.com/articles/temporary-files-and-directories-in-go/)