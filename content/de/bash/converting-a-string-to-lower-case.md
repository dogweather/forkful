---
title:                "Bash: Umwandeln einer Zeichenkette in Kleinschreibung"
simple_title:         "Umwandeln einer Zeichenkette in Kleinschreibung"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Strings in Kleinbuchstaben ist eine nützliche Fähigkeit für jeden, der gerne Bash-Programmierung betreibt. Es ermöglicht eine einfachere und konsistentere Verarbeitung von Texten.

## Anleitung

Um einen String in Kleinbuchstaben umzuwandeln, gibt es mehrere Möglichkeiten in Bash. Eine Möglichkeit ist die Verwendung der integrierten Funktion `tr`, die den Inhalt einer Datei oder eines Strings bearbeiten kann. Die Syntax sieht folgendermaßen aus:

```Bash
echo "HALLO" | tr '[:upper:]' '[:lower:]'
```

Die Ausgabe wäre `hallo`, da `tr` den String `HALLO` in Kleinbuchstaben umgewandelt hat.

Eine weitere Möglichkeit ist die Verwendung des Befehls `sed`, der ähnlich wie `tr` funktioniert. Die Syntax sieht wie folgt aus:

```Bash
echo "WELT" | sed 'y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/'
```

Auch hier wird die Ausgabe `welt` sein.

Es ist auch möglich, eine Schleife zu verwenden, um jeden Buchstaben im String manuell in einen Kleinbuchstaben zu konvertieren. Dies erfordert jedoch etwas mehr Code und ist möglicherweise nicht so effizient wie die beiden oben genannten Methoden.

## Tiefgreifender Einblick

Beim Konvertieren eines Strings in Kleinbuchstaben verwenden sowohl `tr` als auch `sed` die ASCII-Tabelle, um zu bestimmen, welcher Buchstabe in welchen Buchstaben konvertiert werden soll. Dies bedeutet, dass alle Sonderzeichen oder Zeichen aus anderen Sprachen möglicherweise nicht korrekt konvertiert werden. In solchen Fällen ist es möglicherweise besser, eine eigene benutzerdefinierte Funktion zu erstellen, die diese Zeichen berücksichtigt.

## Siehe auch

- Eine umfassende Übersicht über den `tr`-Befehl in Bash: https://www.computerhope.com/unix/utr.htm
- Weitere Informationen zum `sed`-Befehl und seiner Verwendung zur Konvertierung von Texten: https://www.computerhope.com/unix/used.htm