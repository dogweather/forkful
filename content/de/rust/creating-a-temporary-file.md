---
title:                "Rust: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum Temporäre Dateien in Rust erstellen?

Das Erstellen von temporären Dateien ist ein häufiges Szenario in der Programmierung. Es wird oft benötigt, um temporäre Daten oder Zwischenergebnisse zu speichern, die während der Ausführung eines Programms benötigt werden. In Rust können temporäre Dateien auf einfache und effiziente Weise erstellt werden, was sie zu einem nützlichen Werkzeug für Entwickler macht.

## Wie man in Rust temporäre Dateien erstellt

Um eine temporäre Datei in Rust zu erstellen, kann die `tempfile`-Bibliothek verwendet werden. Zunächst muss die Bibliothek in das Projekt eingebunden werden, indem Sie die folgende Zeile in Ihrer `Cargo.toml`-Datei hinzufügen:

```Rust
[dependencies]
tempfile = "3.0.5"
```

Als nächstes muss die Bibliothek in Ihrem Code importiert werden:

```Rust
use tempfile::tempfile;
```

Dann können Sie die Funktion `tempfile()` aufrufen, um eine temporäre Datei zu erstellen. Die Funktion gibt ein `Result`-Objekt zurück, das entweder eine `File`-Instanz oder ein Fehler sein kann. Hier ist ein Beispiel, das eine temporäre Datei erstellt und die Ergebnisse auf der Konsole ausgibt:

```Rust
let temp_file = tempfile();

match temp_file {
    Ok(mut file) => {
        println!("Die temporäre Datei wurde erfolgreich erstellt.");
        //... Weitere Operationen auf der temporären Datei können hier durchgeführt werden ...
    }
    Err(e) => {
        println!("Fehler beim Erstellen der temporären Datei: {}", e);
    }
}
```

Das erstellte `File`-Objekt kann wie eine normale Datei verwendet werden, einschließlich des Schreibens und Lesens von Inhalten. Sobald es nicht mehr benötigt wird, kann es gelöscht werden, indem Sie die `File`-Instanz verwenden und die Funktion `delete()` aufrufen:

```Rust
match file.delete() {
    Ok(()) => println!("Die temporäre Datei wurde gelöscht."),
    Err(e) => println!("Fehler beim Löschen der temporären Datei: {}", e),
}
```

## Tiefer Einblick ins Erstellen von temporären Dateien in Rust

Während die `tempfile`-Bibliothek die Erstellung temporärer Dateien in den meisten Fällen vereinfacht, ist es hilfreich, einige der zugrunde liegenden Mechanismen zu verstehen. In Rust werden temporäre Dateien mithilfe des Betriebssystems erstellt. Die `tempfile`-Bibliothek abstrahiert diese Prozesse und bietet ein Interface, um einfacher auf temporäre Dateien zuzugreifen.

Unter der Haube verwendet die Bibliothek die Funktionen `mkstemp()` oder `mkdtemp()` des Betriebssystems, um eine eindeutige temporäre Datei im System zu erstellen. Die Generierung von eindeutigen Dateinamen wird durch zufällige Zeichenfolgen gewährleistet, die an den vorgegebenen Dateinamen angehängt werden.

Darüber hinaus stellt die Bibliothek Funktionen bereit, um temporäre Dateien in spezifischen Verzeichnissen zu erstellen oder ausgehend von bereits existierenden `File`-Instanzen.

## Siehe auch

- [Dokumentation der `tempfile`-Bibliothek](https://docs.rs/tempfile/3.0.5/tempfile/)
- [Offizielle Rust-Dokumentation zum Arbeiten mit Dateien](https://doc.rust-lang.org/std/fs/)
- [Code-Beispiel für den Umgang mit temporären Dateien in Rust](https://github.com/jcool311/tempfile-rs-example)