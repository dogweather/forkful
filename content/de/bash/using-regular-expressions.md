---
title:                "Die Verwendung regulärer Ausdrücke"
html_title:           "Bash: Die Verwendung regulärer Ausdrücke"
simple_title:         "Die Verwendung regulärer Ausdrücke"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Sich mit regulären Ausdrücken zu beschäftigen, kann eine mächtige Fähigkeit für die Bash-Programmierung sein. Mit regulären Ausdrücken können Sie komplexe Such- und Ersatzmuster definieren, um Texte zu manipulieren und zu verarbeiten.

## So geht's

Die Verwendung von regulären Ausdrücken in der Bash-Programmierung erfordert die Verwendung des `grep` Befehls. Hier ist ein Beispiel, um alle Zeilen zu finden, die das Wort "Haus" enthalten:

```Bash
grep "Haus" textdatei.txt
```

Output:

```
Dies ist eine Zeile mit dem Wort Haus.
Ein anderes Haus, eine andere Zeile.
```

Die `grep`-Option `-E` ermöglicht die Verwendung von erweiterten regulären Ausdrücken, um komplexere Muster zu definieren. Zum Beispiel, um alle Zeilen zu finden, die entweder "Haus" oder "Wohnung" enthalten:

```Bash
grep -E "Haus|Wohnung" textdatei.txt
```

Output:

```
Dies ist eine Zeile mit dem Wort Haus.
Ein anderes Haus, eine andere Zeile.
Und auch eine Zeile mit dem Wort Wohnung.
```

Weitere nützliche `grep`-Optionen sind `-i` für die Suche ohne Beachtung der Groß-/Kleinschreibung und `-o` für die Ausgabe nur der übereinstimmenden Teile der Zeilen.

## Tief eintauchen

Die Verwendung von regulären Ausdrücken erfordert ein gewisses Verständnis einiger Zeichen und Funktionen. Zum Beispiel:

- `^` entspricht dem Anfang einer Zeile, während `$` dem Ende einer Zeile entspricht.
- `.` entspricht genau einem beliebigen Zeichen.
- `[]` ermöglicht die Definition eines Zeichensatzes, z.B. `[abc]` entspricht entweder "a", "b" oder "c".
- `*` entspricht null oder mehr Vorkommen des vorherigen Zeichens.
- `+` entspricht einem oder mehr Vorkommen des vorherigen Zeichens.
- `()` ermöglicht die Gruppierung von Mustern.

Es gibt auch einige hilfreiche Online-Tools, wie z.B. [Regex101] (https://regex101.com/), die Ihnen helfen können, reguläre Ausdrücke zu testen und zu verstehen.

## Siehe auch

* [Bash Regular Expressions](https://www.gnu.org/software/sed/manual/html_node/Bash-regular-expressions.html)
* [The Art of Command Line: Regular Expressions](https://github.com/jlevy/the-art-of-command-line/blob/master/README-de.md#regular-expressions)
* [Einführung in reguläre Ausdrücke in der Bash](https://wiki.ubuntuusers.de/Bash/Regul%C3%A4re_Ausdrucke/)