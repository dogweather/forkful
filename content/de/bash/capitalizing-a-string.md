---
title:                "String in Großbuchstaben umwandeln"
date:                  2024-01-19
html_title:           "C: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"

category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Kapitalisieren eines Strings bedeutet, alle Buchstaben in Großbuchstaben umzuwandeln. Programmierer nutzen dies, um Konsistenz in der Benutzereingabe sicherzustellen oder bestimmte Textelemente hervorzuheben.

## How to:
Capitalizing a string in Bash can be done using `tr`, `awk`, or native Bash parameter expansion. Here's how:

Mit `tr`:
```Bash
echo "kleiner Buchstabe" | tr a-z A-Z
```
Ausgabe:
```
KLEINER BUCHSTABE
```

Mit `awk`:
```Bash
echo "kleiner Buchstabe" | awk '{print toupper($0)}'
```
Ausgabe:
```
KLEINER BUCHSTABE
```

Mit Bash Erweiterung:
```Bash
str="kleiner Buchstabe"
echo "${str^^}"
```
Ausgabe:
```
KLEINER BUCHSTABE
```

## Deep Dive:
Historisch kommt das Kapitalisieren von Strings aus dem Bedürfnis, Texte gleichförmig zu gestalten – beispielsweise in Titeln oder beim Setzen von Umgebungsvariablen in Unix-Systemen. Alternative Methoden, wie die Verwendung von `sed` oder das Umwandeln einzelner Buchstaben mit Hilfe von ASCII-Werten, sind ebenfalls möglich, aber die gezeigten Methoden mit `tr`, `awk` und Bash Erweiterungen sind eleganter und kürzer.

Die `tr`-Methode ist direkt und schnell, perfekt für einfaches Umwandeln. `awk` ist mächtiger und bietet mehr Flexibilität für komplexe Manipulationen. Bash Erweiterungen sind sehr prägnant, jedoch sind ältere Versionen von Bash möglicherweise nicht kompatibel.

Für die Bash-Erweiterung: Das doppelte `^^` wandelt den gesamten String in Großbuchstaben um. Ein einfaches `^` würde nur den ersten Buchstaben ändern.

## See Also:
- GNU `tr` Manual: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- GNU `awk` Manual: https://www.gnu.org/software/gawk/manual/gawk.html
- Bash Parameter Expansion: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
