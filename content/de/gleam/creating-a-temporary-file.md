---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Temporäre Dateien sind kurzlebige Speicher von Daten, die durchgehend während der Ausführung eines Programms aktualisiert werden. Programmierer erstellen sie oft für eine sichere Speicherung zwischen verschiedenen Teilen einer Anwendung oder als Puffer für teure Operationen.

## Wie macht man das:

In Gleam können Sie ein temporäres File mit der `file.temp` Funktion aus dem `gleam/file` Modul erstellen. Hier ist ein einfacher Code-Ausschnitt, der Ihnen zeigt, wie es geht:

```Gleam
import gleam/file.{temp}

fn main() {
  let result = file.temp()
  case result {
    Ok(file) -> 
      let _ = io.println(file.name)
    Error(err) -> 
      let _ = io.println(.error)
  }
}
```

Wenn Sie diesen Code ausführen, wird der Name einer neu erstellten temporären Datei gedruckt.

## Tiefen Tauchgang

Temporäre Dateien sind ein wichtiger Bestandteil älterer und moderner Programmiersprachen und helfen dabei, Daten effizient und sicher zu verwalten.

Einige Gleam-Alternativen zur Verwendung temporärer Dateien können persistente Speicheroptionen wie Datenbanken oder Caches sein. Aber diese Alternativen haben ihre eigenen Kosten und Komplikationen, sodass temporäre Dateien oft die effizienteste Lösung sind.

Unter der Haube verwendet `file.temp` die Erlang-`:file.mkstemp` Funktion zum Erzeugen einer wurzelartigen Datei mit einem eindeutigen Namen in einem sicheren Verzeichnis. Die genaue Implementierung hängt vom zugrunde liegenden Betriebssystem ab.

## Mehr Informationen

Weitere Informationen finden Sie in der [Gleam-Dateidokumentation](https://hexdocs.pm/gleam_stdlib/gleam/file.html) und dem [Erlang-Dateihandbuch](http://erlang.org/doc/man/file.html). Es lohnt sich auch, die anderen Funktionen aus diesem Modul anzu sehen, die Ihnen mehr Kontrolle und Funktionalität bieten.