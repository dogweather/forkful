---
title:                "Überprüfung ob ein Verzeichnis existiert"
html_title:           "Arduino: Überprüfung ob ein Verzeichnis existiert"
simple_title:         "Überprüfung ob ein Verzeichnis existiert"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum

Das Überprüfen, ob ein Verzeichnis existiert, kann in bestimmten Situationen sehr hilfreich sein. Zum Beispiel, wenn Sie sicherstellen möchten, dass eine Datei erfolgreich gespeichert wurde oder wenn Sie dynamisch auf Verzeichnisse zugreifen und diese überprüfen möchten.

## Wie geht's

Um zu überprüfen, ob ein Verzeichnis existiert, müssen Sie die Funktion `FileSystem.exists()` aufrufen und als Parameter den Pfad zu dem Verzeichnis angeben, das Sie überprüfen möchten.

```Arduino
if (FileSystem.exists("/Verzeichnis"))
{
  Serial.println("Das Verzeichnis existiert!");
}
else
{
  Serial.println("Das Verzeichnis existiert nicht!");
}
```

Die Variable `/Verzeichnis` kann dabei auch durch eine Variable oder eine zusammengesetzte Zeichenkette ersetzt werden.

## Tiefergehende Info

Beim Überprüfen von Verzeichnissen gibt es noch einige Dinge zu beachten. So kann es zum Beispiel vorkommen, dass das Verzeichnis nur temporär existiert, wie beim Speichern von Dateien in einem temporären Ordner. In diesem Fall wird die Funktion `exists()` immer `false` zurückgeben, da das Verzeichnis nicht dauerhaft existiert.

Ein weiterer wichtiger Punkt ist, dass die Funktion nur überprüft, ob das Verzeichnis an dem angegebenen Pfad existiert, nicht jedoch ob es auch tatsächlich ein Verzeichnis ist. Um das zu überprüfen, können Sie `FileSystem.isDir()` verwenden.

```Arduino
if (FileSystem.isDir("/Verzeichnis"))
{
  Serial.println("Es ist ein Verzeichnis!");
}
else
{
  Serial.println("Es ist kein Verzeichnis!");
}
```

Natürlich kann auch hier der Pfad wieder durch eine Variable oder Zeichenkette ersetzt werden.

## Siehe auch

Weitere Informationen und Beispiele zum Umgang mit Dateien und Verzeichnissen finden Sie in der offiziellen Arduino Dokumentation:
- [Filesystem API Reference](https://www.arduino.cc/en/Reference/Filesystem)
- [Example: ReadWriteFile](https://www.arduino.cc/en/Tutorial/ReadWriteFile)
- [Example: OpenCloseFile](https://www.arduino.cc/en/Tutorial/OpenCloseFile)