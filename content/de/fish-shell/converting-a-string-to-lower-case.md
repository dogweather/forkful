---
title:                "Umwandlung eines Strings in Kleinschreibung"
html_title:           "Fish Shell: Umwandlung eines Strings in Kleinschreibung"
simple_title:         "Umwandlung eines Strings in Kleinschreibung"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Konvertieren eines Strings zu Kleinbuchstaben ist ein häufig verwendeter Prozess in der Programmierung. Es bezieht sich auf die Umwandlung aller Großbuchstaben in einem String in die entsprechenden Kleinbuchstaben. Programmierer tun dies aus verschiedenen Gründen, wie zum Beispiel um sicherzustellen, dass Benutzereingaben einheitlich behandelt werden oder um Daten für Vergleiche zu standardisieren.

# Wie geht's?
Die Umwandlung eines Strings zu Kleinbuchstaben in der aktuellen Version von Fish Shell ist dank der integrierten Funktion `string tolower` sehr einfach. Hier ist ein einfaches Beispiel:

```
set str "HALLO"
echo (string tolower $str)
```

Die Ausgabe dieses Codes wäre `hallo`, da alle Großbuchstaben in dem ursprünglichen String in Kleinbuchstaben umgewandelt wurden.

Eine andere Möglichkeit, um einen String zu Kleinbuchstaben zu konvertieren, ist die Verwendung des `tr` Befehls zusammen mit der `tr '[:upper:]' '[:lower:]'` Option. Hier ist ein Beispiel:

```
set str "HELLO"
echo $str | tr '[:upper:]' '[:lower:]'
```

Dies würde auch die Ausgabe `hello` erzeugen.

# Tiefere Einblicke
Die Notwendigkeit der Konvertierung von Strings zu Kleinbuchstaben hat eine lange Geschichte in der Programmierung. In älteren Programmiersprachen, die keine eingebauten Funktionen für String-Manipulation hatten, mussten Entwickler komplizierte Methoden verwenden, um dies zu erreichen. Heutzutage gibt es verschiedene Alternativen zu Fish Shell, um dieses Problem zu lösen, wie z.B. GNU Awk oder Perl.

Die `string tolower` Funktion in Fish Shell verwendet die `tr` Befehlsmöglichkeiten `[:upper:]` und `[:lower:]` für die Konvertierung. Diese können auch in Kommandozeilenbefehlen oder Skripten außerhalb von Fish Shell verwendet werden.

# Siehe auch
- Fish Shell Dokumentation: https://fishshell.com/docs/current/cmds/string-tolower.html
- GNU Awk Dokumentation: https://www.gnu.org/software/gawk/manual/html_node/Case-Conversion.html
- Perl Dokumentation: https://perldoc.perl.org/functions/lc.html