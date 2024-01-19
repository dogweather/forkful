---
title:                "Einen String großschreiben"
html_title:           "Bash: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Großschreibung von Strings bezieht sich auf die Umwandlung aller Zeichen in einem Textstring in Großbuchstaben. Programmierer machen das oft für Konsistenz in ihrer Datenausgabe oder um Text leichter lesbar zu machen.

## Wie es geht

Im Scripting mit Bash können sie die eingebaute Funktion `tr` verwenden, um einen String zu kapitalisieren. Hier ist ein einfaches Beispiel:

```Bash
string="hallo welt"
echo "$string" | tr '[:lower:]' '[:upper:]'
```

Die Ausgabe dieses Befehls ist `HALLO WELT`.

## Vertiefung

Historisch gesehen war die Funktion `tr` schon immer ein Teil der UNIX- und Linux-Shell-Skripts, noch bevor es modernere Shells wie Bash gab. 

Es gibt andere Möglichkeiten, Strings in Bash zu kapitalisieren. Zum Beispiel:

```Bash
string="hallo welt"
echo "${string^^}"
```

Diese Methode ist speziell für Bash und funktioniert nicht in anderen Shells.

Die Kapitalisierung von Strings beeinflusst nicht die ursprüngliche Variable, es sei denn, Sie weisen das Ergebnis der Transformation wieder der Variable zu.

## Siehe auch

Bitte checken Sie auch diese nützlichen Links für weitere Informationen:

- GNU `tr` Handbuch: [https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- Bash Beginner's Guide Kapitalisierung: [http://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_03_02.html](http://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_03_02.html)
- Advanced Bash-Scripting Guide `tr` command: [https://tldp.org/LDP/abs/html/string-manipulation.html](https://tldp.org/LDP/abs/html/string-manipulation.html)