---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# String in Kleinbuchstaben Konvertieren in Bash

## Was & Warum?
Das Konvertieren eines Strings in Kleinbuchstaben bedeutet, alle Großbuchstaben in einem Text zu Kleinbuchstaben zu ändern. Programmierer machen dies, um Dateneingaben zu normalisieren und zu vereinfachen, sodass Groß- und Kleinschreibung keine Rolle bei Vergleichen spielt.

## Wie zu:
Hier sind einige Möglichkeiten, dies in Bash zu erreichen:

1. Mit der eingebauten Funktion `tr`:

```Bash
text="Hallo Welt"
echo "${text,,}"
# Ausgabe: hallo welt
```

2. Mit der `tr`-Befehl:

```Bash
text="Hallo Welt"
echo "$text" | tr '[:upper:]' '[:lower:]'
# Ausgabe: hallo welt
```

## Deep Dive
In der Vergangenheit konnten Sie nicht direkt in Bash einen String in Kleinbuchstaben konvertieren, und mussten Pipe und `tr` benutzen. Seit Bash 4.0 gibt es jedoch die eingebaute Funktion `${text,,}`, die diese Aufgabe erledigt.

Es gibt auch Alternativen wie `awk`, `sed` und `perl`:

- Awk:
    
    ```Bash
    echo "Hallo Welt" | awk '{print tolower($0)}'
    # Ausgabe: hallo welt
    ```

- Sed:

    ```Bash
    echo "Hallo Welt" | sed -e 's/\(.*\)/\L\1/'
    # Ausgabe: hallo welt
    ```

- Perl:

    ```Bash
    echo "Hallo Welt" | perl -pe '$_=lc'
    # Ausgabe: hallo welt
    ```

Diese Methoden sind mächtig und bieten weitere Funktionen, aber sie sind auch komplexer und schwieriger zu benutzen als die eingebaute Bash-Funktion.

## Siehe Auch
- [AWK-Befehl in Unix](https://www.geeksforgeeks.org/awk-command-unixlinux-examples/): Ein tiefer Einblick in den `awk`-Befehl.