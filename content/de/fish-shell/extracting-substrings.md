---
title:                "Extrahieren von Teilzeichenketten"
html_title:           "Fish Shell: Extrahieren von Teilzeichenketten"
simple_title:         "Extrahieren von Teilzeichenketten"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Extrahieren von Substrings ist ein Prozess, bei dem Teile eines Strings basierend auf bestimmten Kriterien extrahiert werden. Programmierer nutzen dies, um bestimmte Informationen aus längeren Texten zu erhalten, ohne den gesamten String zu durchsuchen.

## Wie?

In Fish Shell kann das extrahieren von Substrings mit dem `string` Kommando und dem Flag `-s` durchgeführt werden. Zum Beispiel:

```
Fish Shell string -s 2 'Hello World'

Ausgabe: llo World
```

Das Flag `-s` definiert den Startindex, von dem aus die Substring-Extraktion beginnt.

Um die Länge des extrahierten Substrings festzulegen, kann das Flag `-l` verwendet werden. Zum Beispiel:

```
Fish Shell string -l 3 'Hello World'

Ausgabe: Hel
```

Das Flag `-l` gibt an, wie viele Zeichen vom Startindex aus extrahiert werden sollen.

## Tiefere Einblicke

Die Verwendung von Substring-Extraktion ist in vielen Programmiersprachen üblich, um bestimmte Teile von Strings zu erhalten. Alternativ können reguläre Ausdrücke verwendet werden, um ähnliche Ergebnisse zu erzielen. 

Bei der Implementierung von Substring-Extraktion müssen Programmierer darauf achten, dass sie den richtigen Index und die Länge für den zu extrahierenden Teil angeben, um versehentliche Fehler zu vermeiden.

## Siehe auch

Weitere Informationen und Beispiele zur Verwendung von Substring-Extraktion in Fish Shell finden Sie in der offiziellen Dokumentation unter https://fishshell.com/docs/current/cmds/string.html.