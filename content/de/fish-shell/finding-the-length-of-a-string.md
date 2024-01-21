---
title:                "Ermittlung der Zeichenkettenlänge"
date:                  2024-01-20T17:47:14.918279-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ermittlung der Zeichenkettenlänge"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Das Ermitteln der Zeichenkettenlänge bedeutet, zu zählen, wie viele Zeichen in einer Zeichenkette vorhanden sind. Programmierer benötigen diese Information, um Daten zu validieren, Schleifen zu steuern oder Formatierungen vorzunehmen.

## How to: (Wie geht das:)
Ein Beispiel in Fish Shell zur Bestimmung der Länge einer Zeichenkette:

```fish
set my_string "Hallo Welt"
set length (string length $my_string)
echo $length
```

Ausgabe:

```
10
```

## Deep Dive (Tiefer eintauchen)
In der Unix-Welt und in Shell-Scripting im Allgemeinen ist das Zählen von Zeichen in Zeichenketten eine grundlegende Operation. Seit den frühen Tagen des Unix-Systems verwenden Programmierer solche Funktionen, um Textdaten zu verarbeiten. 

In Fish Shell wurde der Befehl `string` relativ spät, genau genommen in der Version 2.3.0, eingeführt. Vorher müssten Benutzer auf externe Tools wie `expr` oder `wc` zurückgreifen oder schlicht in einer Schleife die Zeichen zählen.

Alternativen zum Befehl `string length` könnten so aussehen:

```fish
echo -n $my_string | wc -m
```

oder

```fish
expr length $my_string
```

Implikationen bei der Verwendung des `string` Befehls sind vor allem die Einfachheit und Lesbarkeit, sowie die interne Optimierung, die Fish Shell bietet. Anders als bei der Nutzung des `wc` oder `expr` Befehls, muss hier kein Subprozess gestartet werden, was die Operation effizienter macht.

## See Also (Siehe auch)
Weitere Informationen und Dokumentation über Fish Shell Befehle und deren Nutzung:
- Die offizielle Dokumentation von Fish: [fishshell.com/docs](https://fishshell.com/docs/current/)
- Eine tiefergehende Erklärung der `string` Befehle können unter [fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html) gefunden werden.
- Online-Community und Foren wie Stack Overflow oder die Fish GitHub-Seite für spezifische Fragen und Diskussionen.