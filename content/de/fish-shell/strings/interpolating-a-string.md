---
date: 2024-01-20 17:50:49.970956-07:00
description: "How to: In den fr\xFChen Unix-Tagen: `sh` und `bash` benutzten die Syntax\
  \ `\"$variable\"`. Fish ist moderner und eliminiert oft den Bedarf f\xFCr\u2026"
lastmod: '2024-04-05T22:51:08.831091-06:00'
model: gpt-4-1106-preview
summary: "In den fr\xFChen Unix-Tagen."
title: Zeichenketten interpolieren
weight: 8
---

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
