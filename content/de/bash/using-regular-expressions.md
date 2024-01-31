---
title:                "Einsatz von regulären Ausdrücken"
date:                  2024-01-19
simple_title:         "Einsatz von regulären Ausdrücken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke (RegEx) sind Muster zur Textsuche und -bearbeitung. Programmierer nutzen sie, weil sie mächtige Werkzeuge sind, um komplexe Textaufgaben effizient zu lösen.

## How to:
Hier sind einfache Beispiele für die Nutzung von RegEx in Bash:

```Bash
echo "Die PLZ 10115 gehört zu Berlin." | grep -oP '\b\d{5}\b'
# Ausgabe: 10115

echo "Kontakt: max@beispiel.de" | grep -oP '\S+@\S+'
# Ausgabe: max@beispiel.de

# Dateinamen ausgeben, die mit 'log' enden und eine Nummer enthalten
ls | grep -P 'log\-\d+'
```

## Deep Dive
Reguläre Ausdrücke kamen in den 1950er Jahren auf und wurden im Unix-Tool 'grep' populär. Alternativen zu Bash-RegEx sind Tools wie 'awk' und 'sed'. Beim Implementieren von RegEx sollte man jedoch auf Effizienz achten, da komplexe Muster rechenintensiv sein können.

## See Also
Offizielle GNU-Bash-Dokumentation zu RegEx: https://www.gnu.org/software/bash/manual/
Interaktiver RegEx-Tester: https://regex101.com/
Tutorial zum Umgang mit RegEx in Bash: https://www.tldp.org/LDP/abs/html/regexps.html
