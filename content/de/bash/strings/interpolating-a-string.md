---
date: 2024-01-20 17:50:23.585788-07:00
description: "Anleitung: Stringinterpolation in Bash gibt es schon seit den Anf\xE4\
  ngen der Shell-Programmierung. Anstelle der Stringinterpolation k\xF6nnten Programmierer\u2026"
lastmod: '2024-04-05T21:53:55.926597-06:00'
model: gpt-4-1106-preview
summary: "Stringinterpolation in Bash gibt es schon seit den Anf\xE4ngen der Shell-Programmierung."
title: Zeichenketten interpolieren
weight: 8
---

## Anleitung:
```Bash
#!/bin/bash
name="Welt"
echo "Hallo, $name!"

zahl=7
echo "Die Zahl ist $((zahl * 6))"
```
Ausgabe:
```
Hallo, Welt!
Die Zahl ist 42
```

## Tiefgang:
Stringinterpolation in Bash gibt es schon seit den Anfängen der Shell-Programmierung. Anstelle der Stringinterpolation könnten Programmierer auch mehrere `echo`-Befehle und Stringzusammenfügungen verwenden, aber das ist umständlicher. Bash nutzt `$` für Variablen und `$()` für die Ausführung von Befehlen innerhalb eines Strings. Diese Syntax ist Teil der POSIX-Standardisierung, welche die Kompatibilität zwischen Unix-Betriebssystemen gewährleistet. 

Zu beachten ist, dass es Unterschiede zwischen doppelten (`"`) und einzelnen (`'`) Anführungszeichen gibt – innerhalb von doppelten Anführungszeichen erfolgt die Interpolation, während einzelne Anführungszeichen den String so belassen, wie er ist. Die Verwendung von `{}` um die Variable kann in komplexeren Fällen die Lesbarkeit verbessern und Fehler vermeiden, z.B. `echo "Das Wort ${name}buch"`.

Alternativ gibt es in modernen Scriptsprachen wie Python oder Ruby oft einfachere und mächtigere Interpolationsmechanismen.

## Siehe Auch:
- Bash Manual über Parameter Expansion: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion
- Wiki-Artikel über Shell Scripting: https://en.wikipedia.org/wiki/Shell_script
- Advanced Bash-Scripting Guide zu Quoting: https://tldp.org/LDP/abs/html/quotingvar.html
