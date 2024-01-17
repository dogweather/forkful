---
title:                "Überprüfen, ob ein Verzeichnis existiert"
html_title:           "Gleam: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was ist das und warum sollte man das überprüfen?
Das Überprüfen, ob ein Verzeichnis existiert, ist eine häufige Aufgabe in der Programmierung. Es bedeutet, dass man prüft, ob ein bestimmtes Verzeichnis auf dem Computer vorhanden ist oder nicht. Programmierer tun dies, um sicherzustellen, dass ihr Code fehlerfrei ausgeführt wird und um Probleme zu vermeiden, die auftreten können, wenn ein Verzeichnis nicht existiert.

## Wie geht das?
Hier sind zwei Möglichkeiten, um in Gleam zu überprüfen, ob ein Verzeichnis existiert:

```
// Mit der Funktion `exists` aus der `gleam_std/file` Bibliothek:
let result = file.exists("/pfad/zum/verzeichnis")
match result {
    Ok(exists) -> if exists {
        log.info("Das Verzeichnis existiert!")
    } else {
        log.info("Das Verzeichnis existiert nicht!")
    }
    Err(error) -> log.error(error.message)
}

// Mit dem `file_exists`-Modul aus der `gleam_os/filesystem` Bibliothek:
use gleam_os::filesystem

let result = filesystem.file_exists("/pfad/zum/verzeichnis")
match result {
    Ok(exists) -> if exists {
        log.info("Das Verzeichnis existiert!")
    } else {
        log.info("Das Verzeichnis existiert nicht!")
    }
    Err(error) -> log.error(error)
}
```

Die Ausgabe wird je nachdem, ob das Verzeichnis existiert oder nicht, unterschiedlich sein. Wenn das Verzeichnis existiert, wird die Meldung "Das Verzeichnis existiert!" ausgegeben, ansonsten "Das Verzeichnis existiert nicht!".

## Tiefergehende Informationen
Überprüfen, ob ein Verzeichnis existiert, ist eine wichtige Fähigkeit in der Programmierung, da es dabei hilft, fehlerhaften Code zu vermeiden. Diese Funktion ist auch nützlich, wenn man sicherstellen möchte, dass bestimmte Dateien oder Verzeichnisse vorhanden sind, bevor man mit ihnen arbeitet.

Es gibt verschiedene Möglichkeiten, um in Gleam auf das Dateisystem zuzugreifen, wie zum Beispiel die `gleam_std/file` und die `gleam_os/filesystem` Bibliotheken. Jede Bibliothek bietet verschiedene Funktionen und Methoden, um auf Dateien und Verzeichnisse zuzugreifen.

Bei der Implementierung der Funktion zur Überprüfung, ob ein Verzeichnis existiert, muss man beachten, dass Gleam eine strikt typisierte Sprache ist. Das bedeutet, dass die Funktion immer einen bestimmten Datentyp zurückgeben muss, entweder einen boolean-Wert oder eine Fehlermeldung.

## Siehe auch
Weitere Informationen zum Überprüfen von Dateien und Verzeichnissen in Gleam findet man in der offiziellen Dokumentation unter: [https://gleam.run](https://gleam.run).

Für allgemeine Informationen zur Programmierung mit Gleam empfehlen wir die Lektüre des Buches "Gleam: Functional language for building scalable systems" von Louis Pilfold, das auf [https://pragprog.com](https://pragprog.com/) erhältlich ist.