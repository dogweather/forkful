---
title:                "Gleam: Eine temporäre Datei erstellen"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum

Warum sollte man sich überhaupt die Mühe machen, temporäre Dateien zu erstellen? Ganz einfach: Temporary Files sind nützlich, wenn man Daten zwischen verschiedenen Prozessen oder Programmen austauschen möchte. Sie dienen als eine Art Zwischenspeicher, ähnlich wie der Zwischenablage in Windows.

# Wie geht's

Das Erstellen einer temporären Datei ist in Gleam ganz einfach. Zuerst importieren wir das Modul `gleam/tempfile` und erstellen dann unsere temporäre Datei mit `tempfile.create()`. Hier ein Beispiel:

```Gleam
import gleam/tempfile

let temp_file = tempfile.create()
```

Durch das Aufrufen der Funktion `create()` erhalten wir eine `TempFile` Struktur, welche Informationen über unsere temporäre Datei enthält. Zum Beispiel den Pfad zur Datei über `temp_file.path`. Wir können jetzt diese Datei ganz normal verwenden, ähnlich wie jede andere Datei.

```Gleam
import gleam/tempfile
import gleam/io
import gleam/path

let temp_file = tempfile.create()

// Schreibe etwas in die Datei
io.write(&temp_file.path, "Hallo, Welt!")
```

Mehr Informationen über die `TempFile` Struktur und die verfügbaren Funktionen findest du in der Gleam Dokumentation.

# Tiefer Einblick

Das Erstellen von temporären Dateien kann auch hilfreich sein, wenn man Dateien anlegen möchte, die nur für einen begrenzten Zeitraum verfügbar sein sollen. Nach der Verwendung kann man die temporäre Datei einfach löschen. Zum Beispiel kann man dies in Testumgebungen oder bei der Verarbeitung von sensiblen Daten verwenden.

Außerdem ermöglicht Gleam auch das Erstellen von temporären Verzeichnissen, anstatt nur einzelnen Dateien. Auch hier kann man die `TempDir` Struktur verwenden, um auf das Verzeichnis und dessen Inhalt zuzugreifen.

# Siehe auch

- [Gleam Dokumentation zu `gleam/tempfile`](https://gleam.run/modules/tempfile.html)
- [GitHub Repo von Gleam](https://github.com/gleam-lang/gleam)