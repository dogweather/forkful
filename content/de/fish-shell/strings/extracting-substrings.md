---
aliases:
- /de/fish-shell/extracting-substrings/
date: 2024-01-20 17:45:45.370030-07:00
description: "Das Extrahieren von Teilstrings ist das Herausl\xF6sen bestimmter Zeichen\
  \ oder Zeichenfolgen aus einem l\xE4ngeren String. Programmierer machen das h\xE4\
  ufig, um\u2026"
lastmod: 2024-02-18 23:09:05.311872
model: gpt-4-1106-preview
summary: "Das Extrahieren von Teilstrings ist das Herausl\xF6sen bestimmter Zeichen\
  \ oder Zeichenfolgen aus einem l\xE4ngeren String. Programmierer machen das h\xE4\
  ufig, um\u2026"
title: Teilstrings extrahieren
---

{{< edit_this_page >}}

## What & Why?
Das Extrahieren von Teilstrings ist das Herauslösen bestimmter Zeichen oder Zeichenfolgen aus einem längeren String. Programmierer machen das häufig, um Daten zu parsen, zu validieren oder einfach spezifische Informationen zu isolieren.

## How to:
In Fish Shell kannst du Teilstrings extrahieren, indem du `string` Befehle und Indexierung verwendest. Hier ein paar Beispiele:

```Fish Shell
# Einen Teilstring ab einem bestimmten Index bis zum Ende extrahieren
set mystring "Fish Shell ist fantastisch"
echo $mystring | string sub -s 6
# Output: Shell ist fantastisch

# Einen Teilstring mit einer bestimmten Länge extrahieren
echo $mystring | string sub -s 6 -l 5
# Output: Shell

# Einen Teilstring von einem Index bis zu einem anderen extrahieren
echo $mystring | string sub -s 6 -e 10
# Output: Shell
```

## Deep Dive
In älteren Shells, wie beispielsweise Bash, wurde das Extrahieren von Teilstrings oft durch komplexe Syntax erreicht. In Fish Shell haben wir den `string` Befehl, der die Arbeit mit Strings deutlich vereinfacht. Es gibt auch Alternativen wie `awk` und `sed`, aber `string` ist oft direkter und leichter lesbar.

Die Implementierung erfolgt in Fish über eingebaute Funktionen, die effizient Teile eines Strings basierend auf Start- und Endindizes oder Längenangaben zurückgeben können. Dies ist Teil der übergeordneten Philosophie von Fish, die Kommandozeile benutzerfreundlich und mächtig zu gestalten.

## See Also
- Die offizielle Fish Shell Dokumentation zum `string` Befehl: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Ein Thread über String-Manipulation in Fish bei Stack Overflow: [https://stackoverflow.com/questions/tagged/fish](https://stackoverflow.com/questions/tagged/fish)
