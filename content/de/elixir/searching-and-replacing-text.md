---
title:                "Elixir: Textsuche und -ersetzung"
simple_title:         "Textsuche und -ersetzung"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Haben Sie schon einmal lange Texte bearbeitet und dabei bestimmte Wörter oder Phrasen durch andere ersetzt? Oder mussten Sie schon mal Fehler in Ihrem Code finden und beheben? Mit Elixir können Sie diese Aufgaben schnell und einfach erledigen, dank der Suchen-und-Ersetzen-Funktion.

## Wie funktioniert das?

Die Suchen-und-Ersetzen-Funktion in Elixir basiert auf dem `String.replace/4`-Befehl. Dabei gibt es vier Argumente, die Sie angeben müssen:

- Der ursprüngliche String, in dem gesucht werden soll.
- Das Muster, das gesucht und durch ein anderes Muster ersetzt werden soll.
- Das Muster, durch das das erste Muster ersetzt werden soll.
- Optionen für die Suche und den Ersatz.

Im Folgenden finden Sie ein Beispielcode mit einem Text, in dem wir nach bestimmten Wörtern suchen und diese durch andere ersetzen:

```Elixir
text = "Ich liebe Elixir. Elixir ist so leistungsstark und funktionsreich."
neuer_text = String.replace(text, "Elixir", "Programmiersprache")
```

Die Ausgabe des Codes wäre dann: "Ich liebe Programmiersprache. Programmiersprache ist so leistungsstark und funktionsreich." Wie Sie sehen können, wurden alle Vorkommnisse des Wortes "Elixir" durch "Programmiersprache" ersetzt.

## Tiefer in die Materie eintauchen

Im obigen Beispiel haben wir nur Wörter im String ersetzt, aber es gibt noch viele weitere Optionen, die Sie bei der Suche und dem Ersatz von Text in Elixir verwenden können. Einige dieser Optionen sind:

- Die Option `count`, um anzugeben, wie viele Vorkommnisse ersetzt werden sollen.
- Die Option `regex`, um ein reguläres Ausdrucksmuster zu verwenden.
- Die Option `case_insensitive`, um die Suche nicht auf Groß- und Kleinschreibung zu beschränken.

Sie können auch die Musternutzung bei der Suche einschränken, indem Sie das Muster in Anführungszeichen setzen oder angeben, wo das Muster genau im String vorkommen muss.

## Siehe auch

- Offizielle Elixir-Dokumentation zu `String.replace/4`: https://hexdocs.pm/elixir/String.html#replace/4
- Ein interaktives Tutorial zu Elixir mit Beispielen für die Suche und den Ersatz von Text: https://elixir-lang.org/getting-started/case-cond-and-functions.html#string-replacement

Vielen Dank fürs Lesen und viel Spaß beim Ausprobieren der Suchen-und-Ersetzen-Funktion in Elixir!