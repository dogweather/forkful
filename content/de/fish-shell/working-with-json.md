---
title:                "Arbeiten mit Json"
html_title:           "Fish Shell: Arbeiten mit Json"
simple_title:         "Arbeiten mit Json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?

Wenn du als Programmierer mit Daten in JSON-Format arbeitest, bedeutet das, dass du mit strukturierten Daten in einem bestimmten Textformat umgehst. Es ist ein nützliches Format, um Daten zwischen verschiedenen Systemen auszutauschen, da es plattformunabhängig und leicht lesbar ist. Programmierer verwenden JSON, um Daten aus APIs (Application Programming Interfaces), Datenbanken oder Dateien zu lesen und zu schreiben.

## So geht's:

Mit der aktuellen Version von Fish Shell kannst du ganz einfach mit JSON-Daten arbeiten. Hier sind ein paar Beispiele, wie du das tun kannst:

### JSON-Daten lesen:
```
Fish Shell -c 'set data (jq .foo bar.json)'
```
Hier setzt du die Variable "data" auf den Wert von dem JSON-Element "foo" aus der Datei "bar.json". Du brauchst dazu allerdings das Tool "jq", um den Befehl auszuführen.

### JSON-Daten schreiben:
```
Fish Shell -c 'echo {"foo": "bar"} | jq .foo > bar.json'
```
Hier schreibst du die Daten "foo" und "bar" in das JSON-Format und speicherst es in der Datei "bar.json".

### Mehrere JSON-Elemente auslesen:
```
Fish Shell -c 'set data (cat bar.json | jq ".[].foo")'
```
Hier werden alle Werte des JSON-Elements "foo" aus der Datei "bar.json" in der Variable "data" gespeichert.

## Tiefere Einblicke:

Die erste Version von JSON wurde im Jahr 2001 veröffentlicht und war eine Alternative zu XML, einem anderen Datenformat. Heutzutage ist JSON eines der am häufigsten verwendeten Formate für den Datenaustausch in Webanwendungen.

Es gibt auch andere Tools und Bibliotheken, die dir helfen können, mit JSON-Daten zu arbeiten, wie zum Beispiel "jq" und "json.sh". Diese können dir helfen, JSON-Daten zu formatieren, zu filtern und zu validieren.

Die Implementation von JSON in Fish Shell nutzt die "jq"-Bibliothek im Hintergrund, um die Daten zu verarbeiten. Wenn du also mit JSON-Daten arbeitest, musst du sicherstellen, dass du "jq" installiert hast.

## Siehe auch:

- [Die offizielle Website für JSON](https://www.json.org/)
- [Die offizielle Website für Fish Shell](https://fishshell.com/)
- [Die offizielle Website für "jq"](https://stedolan.github.io/jq/)