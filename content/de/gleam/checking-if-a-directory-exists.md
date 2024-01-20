---
title:                "Überprüfung, ob ein Verzeichnis existiert"
date:                  2024-01-20T14:56:12.650872-07:00
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis existiert, bedeutet zu checken, ob ein bestimmter Pfad auf dem Filesystem zu einem realen Ordner führt. Programmierer machen das, um Fehlern vorzubeugen, wenn sie auf Dateien in diesem Verzeichnis zugreifen oder Aktionen ausführen wollen.

## How to:
Hier ein einfaches Beispiel in Gleam, wie man prüft, ob ein Verzeichnis existiert:

```gleam
import gleam/io
import gleam/erlang

pub fn check_directory_exists(dir_path: String) -> Bool {
  erlang.is_directory(dir_path)
}

pub fn main() {
  let directory = "/path/to/directory"
  let exists = check_directory_exists(directory)
  io.println(exists)
}
```

Sample Output:

```
true
```

## Deep Dive
Das Prüfen, ob ein Verzeichnis existiert, ist ein klassisches Problem, das schon seit den frühen Tagen der Systemprogrammierung existiert. Alternativen für diesen Vorgang in anderen Sprachen sind oft mit Funktionen wie `os.path.exists()` in Python oder `fs.existsSync()` in Node.js möglich. Durch die Entwicklung der Sprach-APIs wurde dieser Prozess vereinfacht, und in Gleam wird er durch die Überbrückung zur Erlang-Runtime realisiert, da Gleam noch keine eigene IO-Bibliothek für solche systemnahen Operationen hat.

## See Also
- Erlang's File Module: [Erlang -- file](http://erlang.org/doc/man/file.html)
- Filesystem Handling in Other Languages: [Python's os.path](https://docs.python.org/3/library/os.path.html), [Node.js' fs](https://nodejs.org/api/fs.html)