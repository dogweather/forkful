---
title:    "Gleam: Ein neues Projekt beginnen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich dazu entscheiden, ein neues Projekt mit Gleam zu starten? Nun, Gleam ist eine funktionale und statisch typisierte Programmiersprache, die es Entwicklern ermöglicht, sichere und robuste Anwendungen zu erstellen. Mit Gleam können wir vermeiden, dass unerwartete Fehler auftreten, indem wir unser Programm bereits zur Compile-Zeit überprüfen und Fehler auffinden. Außerdem ist Gleam eine relativ junge Sprache, die ständig weiterentwickelt und verbessert wird, was es zu einem aufregenden Bereich für Entwickler macht.

## Wie man anfängt

Um mit Gleam zu beginnen, müssen wir zunächst die Gleam Runtime und den Compiler installieren. Dann erstellen wir eine neue Datei mit der Erweiterung ".gleam" und können mit dem Schreiben unseres Codes beginnen.

```Gleam
// Hello World Beispiel
pub fn main() {
   let message = "Hallo, Welt!"
   IO.println(message)
}
```

In diesem Beispiel definieren wir eine Hauptfunktion, die eine Nachricht speichert und mit Hilfe von IO.println auf der Konsole ausgibt. Um den Code auszuführen, können wir den Gleam-Compiler verwenden und dann die generierte ausführbare Datei starten.

## Tiefergehende Informationen

Bevor wir mit der Programmierung in Gleam loslegen, ist es wichtig zu wissen, dass Gleam auf der Erlang VM läuft und somit von der umfangreichen Standardbibliothek und den Leistungsmerkmalen von Erlang profitiert. Wir können auch Gleam-Module in andere Sprachen wie Elixir oder Ruby einbinden, was die Anwendungsmöglichkeiten von Gleam erweitert.

Um ein neues Gleam-Projekt zu starten, können wir den Build-Manager Rebar3 verwenden, der automatisch die notwendigen Dateien und Befehle erstellt. Von dort aus können wir unsere Anwendung mithilfe der zahlreichen von Gleam unterstützten Werkzeuge wie Tests, Formatierung und Dokumentation weiter verbessern.

## Siehe auch

- [Offizielle Gleam Dokumentation](https://gleam.run/getting-started/)
- [Erlang Homepage](https://www.erlang.org/)
- [Rebar3 Homepage](https://www.rebar3.org/)