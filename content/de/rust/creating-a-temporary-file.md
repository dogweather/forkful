---
title:    "Rust: Erstellung einer temporären Datei"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum
Das Erstellen temporärer Dateien ist in vielen Programmen ein wichtiger Bestandteil. Es ermöglicht die temporäre Speicherung von Daten, ohne dass dauerhafte Dateien erstellt werden müssen. Dadurch können Programme effizienter arbeiten und Speicherplatz wird optimal genutzt.

## Wie funktioniert das?
Das Erstellen einer temporären Datei in Rust ist ein einfacher Prozess. Zunächst muss das Modul "std::fs" importiert werden. Dann kann die Funktion "std::fs::tempfile()" aufgerufen werden, um eine neue temporäre Datei zu erstellen.

```Rust
use std::fs;

let temp_file = fs::tempfile().expect("Konnte keine temporäre Datei erstellen.");
```

Die Funktion gibt ein Objekt vom Typ "std::fs::File" zurück, das auf die erstellte temporäre Datei verweist. Nun können Daten in diese Datei geschrieben werden, indem die Funktionen "write()" oder "write_all()" aufgerufen werden.

```Rust
temp_file.write(b"Hallo Welt!").expect("Konnte nicht in die Datei schreiben.");
```

Das "b" vor dem String zeigt an, dass es sich um einen Byte-String handelt. Dies ist notwendig, da die Funktionen "write()" und "write_all()" nur mit Byte-Daten arbeiten.

## Tiefgründige Analyse
Wenn eine temporäre Datei erstellt wird, wird sie automatisch im vom System vorgegebenen temporären Verzeichnis gespeichert. Dieses Verzeichnis kann mithilfe der Funktion "tmpdir()" aus dem Modul "std::fs" abgerufen werden.

```Rust
let temp_dir = fs::tmpdir();
println!("Temporäres Verzeichnis: {}", temp_dir.to_str().unwrap());
```

Es ist auch möglich, die Datei in einem bestimmten Verzeichnis zu erstellen, indem man der Funktion "tempfile()" einen Pfad als Argument übergibt.

```Rust
fs::tempfile("/home/benutzer/temp/datei.txt");
```

Es ist wichtig zu beachten, dass temporäre Dateien automatisch gelöscht werden, sobald sie aus dem Speicherbereich des Programms entfernt werden. Dies geschieht normalerweise beim Schließen des entsprechenden File-Objekts oder beim Beenden des Programms. Wenn jedoch eine temporäre Datei dauerhaft gespeichert werden soll, muss sie explizit umbenannt werden.

## Siehe auch
- [Offizielle Rust Dokumentation zu temporären Dateien](https://doc.rust-lang.org/std/fs/fn.tempfile.html)
- [Weitere Informationen zu std::fs-Modul](https://doc.rust-lang.org/std/fs/index.html)
- [Diskussion über die Verwendung von temporären Dateien in Rust](https://users.rust-lang.org/t/creating-a-temporary-file/10524)