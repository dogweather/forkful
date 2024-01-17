---
title:                "Ein neues Projekt beginnen"
html_title:           "Gleam: Ein neues Projekt beginnen"
simple_title:         "Ein neues Projekt beginnen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Was & Warum?

Ein neues Projekt zu starten bedeutet, dass man als Programmierer einen neuen Code schreibt, der ein bestimmtes Ziel oder Problem löst. Programmierer machen dies, um innovative Ideen umzusetzen, Probleme zu lösen oder verbesserte Funktionen für bestehende Projekte zu erstellen.

# Wie geht es?

Um ein neues Projekt in Gleam zu starten, müssen Sie zunächst eine Projektstruktur mit `gleam new` erstellen. Dann können Sie die verschiedenen Module und Funktionen mit `gleam gen` generieren und sie mit Ihren eigenen Ideen und Lösungen implementieren.

```
Gleammaker@MacBookAir ~ % gleam new my_project
==> Erstelle neues Projekt my_project
Gleammaker@MacBookAir ~ % cd my_project
Gleammaker@MacBookAir org_name ~/my_project % gleam gen module MyModule
==> Erstelle neues Modul MyModule
```

Um Ihre Module und Funktionen zu testen, können Sie `gleam build` verwenden, um eine ausführbare Datei zu erstellen, und dann `gleam test` ausführen, um Ihre Tests zu überprüfen. Hier ist ein Beispiel für eine Funktion, die zwei Zahlen addiert:

```
fn add(x, y) {
    x + y
}

test "Addition von zwei Zahlen sollte das richtige Ergebnis zurückgeben" {
    expect(add(2, 3)).toBe(5)
}
```

```
Gleammaker@MacBookAir ~ % gleam build add_test
Gleammaker@MacBookAir ~ % gleam test add_test.beam
==> False is True
==> All tests passed
```

# Tief bettucht

Gleam ist eine funktionale, statisch typisierte Sprache, die auf Erlang basiert. Sie wurde entwickelt, um einfache, stabile und parallele Anwendungen zu erstellen. Alternativen zu Gleam sind unter anderem Elixir, Clojure und Haskell. Gleam unterstützt auch die Integration mit bestehenden Erlang-Code.

# Siehe auch

- Offizielle Gleam-Dokumentation: https://gleam.run/
- Erlang-Programmiersprache: https://www.erlang.org/
- Andere funktionale Programmiersprachen: https://www.toptal.com/developers/blog/functional-programming-languages