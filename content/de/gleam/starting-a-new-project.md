---
title:                "Gleam: Ein neues Projekt starten"
programming_language: "Gleam"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

Gleam Programmierung für Anfänger – Start eines neuen Projekts

## Warum

Warum sollte man überhaupt ein neues Projekt starten? Es gibt verschiedene Gründe, warum man sich mit der Programmiersprache Gleam beschäftigen sollte. Zum einen bietet sie eine statische Typisierung, die zu einer besseren Fehlererkennung und einem insgesamt robusteren Code führt. Zudem ist Gleam durch die Verwendung der funktionalen Programmierung gut für Parallelisierung geeignet. Auch die Unterstützung für Multi-Paradigma-Programmierung macht Gleam zu einer interessanten Sprache für Entwickler aller Erfahrungsstufen.

## Wie man startet

Um ein neues Projekt in Gleam zu starten, gibt es verschiedene Möglichkeiten. Eine davon ist die Verwendung des Tools "rebar3", welches als Build- und Projektverwaltungstool für Gleam-Projekte dient. Um ein neues Projekt zu erstellen, muss man zunächst das Kommando `rebar3 new gleam_project` ausführen. Dies erstellt eine neue Gleam-Projektstruktur mit den notwendigen Dateien und Ordnern. Anschließend kann man das Projekt mit `rebar3 compile` compilieren und mit `rebar3 shell` ausführen.

```Gleam
fn main() {
  IO.println("Hallo, Welt!")
}
```

Nach der Ausführung dieses Codes sollte die Ausgabe "Hallo, Welt!" erscheinen. Dies ist ein einfaches Beispiel, um sicherzustellen, dass die Gleam-Umgebung korrekt eingerichtet wurde.

## Eintauchen ins Projekt

Wenn man tiefer in die Welt von Gleam eintauchen möchte, gibt es einige wichtige Aspekte zu beachten. Zum einen handelt es sich bei Gleam um eine funktionale Programmiersprache, was bedeutet, dass Funktionen als First-Class-Citizens betrachtet werden. Das heißt, sie können als Parameter an andere Funktionen übergeben und von diesen zurückgegeben werden.

Eine weitere wichtige Eigenschaft ist die strikte statische Typisierung. Das bedeutet, dass jeder Wert in Gleam einen festen Datentyp hat und dieser zur Kompilierzeit überprüft wird. Dies führt zu weniger Fehlern während der Laufzeit und zu einem insgesamt robusteren Code.

Es gibt auch viele Bibliotheken, die für die Verwendung in Gleam- Projekten verfügbar sind, von Datenbanktreibern bis hin zu Web-Frameworks. Es lohnt sich, die verschiedenen Möglichkeiten zu erkunden und herauszufinden, welche am besten zu Ihrem Projekt passt.

## Siehe auch

- [Offizielle Gleam Dokumentation](https://gleam.run/)
- [Gleam Beispiele](https://github.com/gleam-lang/gleam/)
- [Gleam Community Forum](https://elixirforum.com/c/gleam-lang/)