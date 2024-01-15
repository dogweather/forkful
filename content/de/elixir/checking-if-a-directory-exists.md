---
title:                "Überprüfen, ob ein Verzeichnis vorhanden ist"
html_title:           "Elixir: Überprüfen, ob ein Verzeichnis vorhanden ist"
simple_title:         "Überprüfen, ob ein Verzeichnis vorhanden ist"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Schritt in der Elixir-Programmierung, um sicherzustellen, dass das Programm reibungslos ausgeführt wird und keine Fehler auftreten. Es hilft auch dabei, sicherzustellen, dass die benötigten Daten oder Dateien vorhanden sind, bevor das Programm ausgeführt wird.

## So geht's

Die Überprüfung, ob ein Verzeichnis existiert, kann in Elixir auf verschiedene Weise erfolgen, je nachdem, welches Modul Sie verwenden möchten. Hier sind zwei Möglichkeiten, um dies zu erreichen:

```Elixir
# Mit dem File-Modul
File.ls?("/pfad/zum/verzeichnis")

# Mit dem Path-Modul
Path.directory?("/pfad/zum/verzeichnis")
```

Die obigen Beispiele zeigen, wie Sie das `ls?` - oder `directory?` - Funktionen aus den `File` oder `Path` Modulen verwenden können, um zu überprüfen, ob das Verzeichnis existiert. Beide Funktionen geben `true` zurück, wenn das Verzeichnis vorhanden ist, andernfalls geben sie `false` zurück.

## Tiefere Einblicke

Wenn Sie sich fragen, wie Elixir das Verzeichnis überprüft, können Sie einen tieferen Einblick in den Quellcode werfen. Beide Funktionen rufen die `_exist?` Funktion des `:file` Moduls auf, die letztendlich das `:file.info` Bif verwendet, um zu überprüfen, ob das Verzeichnis vorhanden ist.

Zusätzlich können Sie mit der `:file.ls` Bif eine Liste der Dateien und Verzeichnisse in einem angegebenen Pfad erhalten. Diese Funktion gibt eine leere Liste zurück, wenn das Verzeichnis nicht existiert.

## Siehe auch

- [Elixir File Modul Dokumentation](https://hexdocs.pm/elixir/File.html)
- [Elixir Path Modul Dokumentation](https://hexdocs.pm/elixir/Path.html)
- [Elixir :file Modul Dokumentation](https://hexdocs.pm/elixir/File.html)