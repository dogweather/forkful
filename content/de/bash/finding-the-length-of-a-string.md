---
title:                "Ermittlung der Zeichenkettenlänge"
date:                  2024-01-20T17:46:44.611603-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ermittlung der Zeichenkettenlänge"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Länge eines Strings messen heißt, zu zählen, wie viele Zeichen er enthält. Das brauchen Programmierer, um Eingaben zu validieren, Textdaten zu verarbeiten oder Speicherplatzbedarf abzuschätzen.

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