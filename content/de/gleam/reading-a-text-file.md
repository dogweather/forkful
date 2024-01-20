---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Gleam Programmieren: Eine Textdatei lesen

## Was & Warum?

Das Lesen einer Textdatei ist der Prozess, bei dem wir Daten aus einer Datei extrahieren. Es ist eine gebräuchliche Aufgabe beim Programmieren, da es uns ermöglicht, Daten zu überprüfen, zu analysieren oder zu manipulieren.

## Wie macht man das:

In Gleam können wir eine Textdatei lesen mit der Funktion `gleam/otp`:

```gleam
import gleam/otp.{file}

fn main(args: List(String)) {
   let result = file.read("meine_datei.txt") // Pfad zur Datei
   case result {
       Ok(file_content) -> 
          io.println(file_content)
       Error(_) -> 
          io.println("Ein Fehler ist aufgetreten beim Lesen der Datei.")
   }
}
```

Ausführung des Codes gibt den Textinhalt der Datei "meine_datei.txt" aus, oder einen Fehlermeldung wenn das Lesen der Datei nicht erfolgreich war.

## Tiefere Einblicke:

Historisch gesehen haben frühere Programmiersprachen wie C oder Java umfangreichere Verfahren zum Lesen von Textdateien. Sie erfordern das Öffnen der Datei, das Lesen der Datei Zeile für Zeile und dann das Schließen der Datei. Gleam vereinfacht diesen Prozess.

Es gibt alternative Wege zum Lesen von Dateien in Gleam, zum Beispiel könnte man `gleam/otp.stream` verwenden, um eine große Datei zu lesen, wenn der Speicher begrenzt ist.

Beim Lesen einer Datei wird intern ein "File Descriptor" verwendet. Dies ist eine Referenz auf das Betriebssystem, die den Zugriff auf die Datei steuert. Dies ist jedoch in der facebenen Gleam-API verborgen und wird nur selten benötigt.

## Siehe auch:

Gleam Documentation: https://gleam.run/documentation

Reading and Writing Files in Gleam: https://github.com/gleam-lang/stdlib/tree/main/src/gleam/otp