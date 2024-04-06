---
date: 2024-01-20 17:46:44.611603-07:00
description: "So geht's: Historisch gesehen gab es in den fr\xFChen Tagen der Shell-Programmierung\
  \ keine eingebaute Funktionalit\xE4t, um die L\xE4nge eines Strings zu messen.\u2026"
lastmod: '2024-04-05T21:53:55.931653-06:00'
model: gpt-4-1106-preview
summary: "Historisch gesehen gab es in den fr\xFChen Tagen der Shell-Programmierung\
  \ keine eingebaute Funktionalit\xE4t, um die L\xE4nge eines Strings zu messen."
title: "Ermittlung der Zeichenkettenl\xE4nge"
weight: 7
---

## So geht's:
```Bash
# String-Länge mit ${#string}
str="Hallo, Welt!"
echo ${#str}
```

Ausgabe:
```
13
```

```Bash
# Länge eines Strings in einer Variablen
length=${#str}
echo $length
```

Ausgabe:
```
13
```

## Tiefergehend:
Historisch gesehen gab es in den frühen Tagen der Shell-Programmierung keine eingebaute Funktionalität, um die Länge eines Strings zu messen. Man nutzte `expr` oder externe Programme wie `awk`. Im Laufe der Zeit wurde Bash erweitert, um solche Aufgaben direkt erledigen zu können.

Alternativen zur Bash-eigenen Methode sind Kommandos wie `wc` oder `expr`, wobei diese einen Umweg und mehr Ressourcen bedeuten können:

```Bash
echo -n $str | wc -c
```

Die Implementierung mit `${#string}` ist direkt ins Bash eingebaut, effizient und schnell. Sie zählt tatsächlich die Zeichen, was bei Multibyte-Zeichen wie im UTF-8-Kodierung wichtig ist.

## Siehe auch:
- Bash Manual: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
- Stack Overflow: Diskussionen über das Zählen von Stringlängen und spezielle Fälle: https://stackoverflow.com/
