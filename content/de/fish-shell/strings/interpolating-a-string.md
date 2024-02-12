---
title:                "Zeichenketten interpolieren"
aliases: - /de/fish-shell/interpolating-a-string.md
date:                  2024-01-20T17:50:49.970956-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten interpolieren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Interpolieren, also das Einfügen von Variablen in Strings, spart dir viel Tipparbeit und vereinfacht die Code-Wartung. Es ermöglicht dynamische Textausgaben, was essentiell ist für Skripte, die auf Nutzereingaben oder ändernden Daten operieren.

## How to:
Fish Shell macht's leicht:

```Fish Shell
set name "Welt"
echo "Hallo, $name!"  # Variable wird direkt im String eingefügt
```
Ausgabe:
```
Hallo, Welt!
```

Mit einer kleinen Twist - braced syntax für Mehrdeutigkeiten:

```Fish Shell
set mood "glücklich"
echo "Ich bin {$mood}lich."  # Klammern klären, wo die Variable endet
```
Ausgabe:
```
Ich bin glücklichlich.
```

## Deep Dive
In den frühen Unix-Tagen: `sh` und `bash` benutzten die Syntax `"$variable"`. Fish ist moderner und eliminiert oft den Bedarf für Anführungsstriche.

Alternativen? Sicher, manche benutzen `printf` für komplexere Aufgaben:

```Fish Shell
set tier "Fische"
printf "Ich mag %s.\n" $tier  # %s steht für eine String-Variable
```

Bei der Umsetzung: Fish evaluiert den String und ersetzt Variablen durch deren Werte. Achte auf Sonderzeichen - manchmal willst du sie wörtlich nehmen (dann verwende Single Quotes).

## See Also
Fish Dokumentation: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)

Einen allgemeinen Guide zu String Interpolation: [https://en.wikipedia.org/wiki/String_interpolation](https://en.wikipedia.org/wiki/String_interpolation)

Ein direkter Vergleich verschiedener Shells: [https://wiki.ubuntuusers.de/Shell/Stringverarbeitung/](https://wiki.ubuntuusers.de/Shell/Stringverarbeitung/)
