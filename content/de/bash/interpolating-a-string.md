---
title:                "String-Interpolation"
html_title:           "Bash: String-Interpolation"
simple_title:         "String-Interpolation"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

Was ist "String-Interpolation" und warum machen Programmierer es?

Die Methode der String-Interpolation bezieht sich darauf, Variablen in einen String einzufügen, anstatt den String zu hardcoden. Programmierer nutzen dies, um die Lesbarkeit und Flexibilität ihres Codes zu verbessern.

Wie funktioniert das in Bash?

String-Interpolation in Bash kann auf verschiedene Weisen durchgeführt werden. Hier sind zwei Beispiele:

```Bash
name="Max"
echo "Hallo, mein Name ist $name." 
```
Output: Hallo, mein Name ist Max.

```Bash
year=$(date +%Y)
echo "Wir leben im Jahr $((year + 1))."
```
Output: Wir leben im Jahr 2022.

Tiefere Einblicke

Die Idee der String-Interpolation stammt aus der Programmiersprache Lisp, aber wird in vielen anderen Sprachen verwendet, einschließlich Bash. Eine alternative Methode ist die Konkatenation, bei der Strings aneinandergehängt werden, um eine neue Zeichenkette zu bilden. In Bash gibt es auch die Möglichkeit, Variablen in Strings mit dem "${...}" Format zu suchen.

Weiterführende Informationen

Eine umfassende Erklärung zum Thema String-Interpolation in Bash findest du auf der offiziellen Dokumentation: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html.

Weitere Informationen zum Thema der Variablensubstitution findest du hier: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Shell-Parameter-Expansion.

In diesem Artikel erfährst du mehr über die Entwicklungsgeschichte von Bash: https://www.gnu.org/software/bash/manual/bash.html#Introduction.