---
title:                "Suchen und Ersetzen von Text"
html_title:           "C#: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Suche und Ersetzung von Text ist eine grundlegende Operation in der Programmierung - sie ermöglicht das Auffinden spezifischer Zeichenketten innerhalb eines Textes und ihre mögliche Ersetzung mit etwas Anderem. Programmierer nutzen es ständig, zum Beispiel bei der Bearbeitung von Textdaten oder bei der modernisierung veralteten Codes.

## Wie es geht:
In Gleam können Sie die eingebaute `replace` Funktion nutzen, um Text zu suchen und zu ersetzen. Hier ist ein einfacher Beispielcode und seine Ausgabe:

```gleam
import gleam/string

fn main() {
  let original = "Hallo Welt!"
  let replaced = string.replace(original, "Welt", "Gleam")

  assert replaced == "Hallo Gleam!"
}
```

Führen Sie es aus und Sie werden "Hallo Gleam!" als Ausgabe sehen.

## Tiefen Tauchgang
Textersetzungen sind nichts Neues in der Programmierung. Bereits in den allererst Line-Editors aus den 1960er Jahren gab es Funktionen für Textsuche und -ersatzung. Heutzutage gibt es zahlreiche Methoden und Bibliotheken für solche Operationen, von regex-basierten Funktionen bis hin zu umfassenden textverarbeitungsbibliotheken.

In Gleam wird `replace` als native Funktion in der `gleam/string` Bibliothek geliefert, die aus Leistungssicht optimiert ist. Sie ist effizient und einfach zu bedienen, was sie zu einer idealen Wahl für Gleam-Entwickler macht.

## Siehe auch
Weitere relevante Ressourcen für das Arbeiten mit Text in Gleam sind:

1. Die vollständige [Gleam String API Dokumentation](https://hexdocs.pm/gleam_stdlib/gleam/string/)
2. Mehr über Textverarbeitung in der Informatik auf [Wikipedia](https://de.wikipedia.org/wiki/Textverarbeitung_(Informatik))