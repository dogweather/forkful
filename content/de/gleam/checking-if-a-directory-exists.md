---
title:                "Gleam: Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum
Das Überprüfen, ob ein Verzeichnis existiert, ist eine wichtige Aufgabe beim Programmieren. Es ermöglicht einem, sicherzustellen, dass alle erforderlichen Dateien und Ordner vorhanden sind, bevor eine Operation ausgeführt wird.

## Wie es funktioniert
```Gleam
import gleam/io

fn main() {
    let dir = "/pfad/zum/verzeichnis"
    let exists = io.exists(dir)
    if exists {
        io.print("Verzeichnis existiert")
    } else {
        io.print("Verzeichnis existiert nicht")
    }
}
```

Das obige Beispiel zeigt, wie einfach es ist, mit Gleam auf die Existenz eines Verzeichnisses zu prüfen. Die `exists` Funktion aus dem `io` Modul gibt `true` zurück, wenn das angegebene Verzeichnis vorhanden ist, andernfalls `false`.

## Tiefer eintauchen
Beim Überprüfen der Verzeichnisexistenz gibt es einige wichtige Dinge zu beachten. Zum Beispiel sollte man sicherstellen, dass man auf das richtige Verzeichnis zugreift, indem man absolute Pfade verwendet. Außerdem ist es wichtig, die Ausnahmebehandlung zu implementieren, falls das Verzeichnis nicht gefunden werden kann.

Ein weiterer wichtiger Aspekt ist, dass Gleam standardmäßig nur auf lokale Dateisysteme zugreift. Falls man auf das Dateisystem eines anderen Computers oder Servers zugreifen möchte, empfiehlt es sich, einen Bibliothek wie z.B. `gleamfs` zu nutzen.

## Siehe auch
- [Gleam `io` Modul Dokumentation](https://gleam.run/modules/io)
- [Gleam `gleamfs` Bibliothek](https://github.com/lpil/gleamfs)