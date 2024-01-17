---
title:                "Das Schreiben in den Standardfehler"
html_title:           "Elixir: Das Schreiben in den Standardfehler"
simple_title:         "Das Schreiben in den Standardfehler"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Was & Warum?

Schreiben auf den Standardfehler (standard error) ist ein Weg für Programmierer, um Fehler- und Warnungsmeldungen auszugeben. Standardfehler ist ein Kanal, der speziell für das Senden von Ausgaben reserviert ist, die Fehler oder Warnungen enthalten. Dadurch kann der Benutzer unterscheiden, ob die Ausgabe normale Programmausgaben oder tatsächliche Fehlermeldungen sind.

## So geht's:

```Elixir
IO.puts("Normale Ausgabe") # Gibt "Normale Ausgabe" auf den Standardausgang (standard output) aus
IO.puts(:stderr, "Fehlermeldung") # Gibt "Fehlermeldung" auf den Standardfehler (standard error) aus
```
Ausgabe:
```
Normale Ausgabe
Fehlermeldung (in rot oder farbig markiert, je nach Konsole oder Umgebung)
```

## Tiefere Einblicke:

Das Schreiben auf den Standardfehler hat seinen Ursprung im Unix-Betriebssystem und wurde später in anderen Systemen übernommen. Alternativ können Programmierer auch auf den Standardausgang schreiben und die Ausgabe manuell formatieren, um Fehlermeldungen hervorzuheben. In Elixir gibt es auch die Funktion `Logger.error/1`, die zur Ausgabe von Fehlern verwendet werden kann.

## Siehe auch:

- Offizielle Elixir-Dokumentation zu IO
- Stack Overflow-Eintrag zu Standardfehler auf Unix-Systemen