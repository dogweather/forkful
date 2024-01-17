---
title:                "Verwendung regulärer Ausdrücke"
html_title:           "Bash: Verwendung regulärer Ausdrücke"
simple_title:         "Verwendung regulärer Ausdrücke"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

Was sind reguläre Ausdrücke und warum brauchen Programmierer sie?

Reguläre Ausdrücke sind eine Möglichkeit, Textzeichenfolgen zu definieren und zu suchen. Programmierer benutzen sie, um Muster in Texten zu identifizieren und bestimmte Aufgaben automatisch auszuführen.

Wie geht das?

In Bash können reguläre Ausdrücke mit dem Befehl `grep` verwendet werden. Ein Beispiel für die Verwendung von `grep` mit regulären Ausdrücken ist die Suche nach allen Zeilen in einer Textdatei, die das Wort "Hallo" enthalten.

```Bash
grep "Hallo" textdatei.txt
```

Das Ergebnis dieser Suche wird alle Zeilen in der Textdatei zeigen, die das Wort "Hallo" enthalten.

Eine vertiefende Betrachtung

Reguläre Ausdrücke wurden in den 1950er Jahren von den Mathematikern Stephen Cole Kleene und Peter John Landin entwickelt. Sie wurden später von dem Informatiker Ken Thompson in der Programmiersprache `ed` implementiert und sind seitdem in vielen Programmiersprachen verfügbar.

Alternativen zu regulären Ausdrücken in Bash sind die Befehle `sed` und `awk`, die ebenfalls Textverarbeitungsfunktionen bieten.

Nützliche Links

Eine ausführlichere Einführung in reguläre Ausdrücke in Bash findest du auf der offiziellen Bash-Website: https://www.gnu.org/software/bash/manual/html_node/Pattern-Matching.html

Weitere Informationen zu `grep`, `sed` und `awk` findest du hier:

- https://www.gnu.org/software/grep/
- https://www.gnu.org/software/sed/
- https://www.gnu.org/software/gawk/