---
title:                "Schreiben auf den Standardfehler"
html_title:           "Bash: Schreiben auf den Standardfehler"
simple_title:         "Schreiben auf den Standardfehler"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man überhaupt auf die Standardfehlerausgabe schreiben? Nun, das hat mehrere Gründe. Zum einen kann es helfen, Fehler in einem Bash-Skript zu identifizieren und zu debuggen. Zum anderen kann es auch nützlich sein, um Benutzer über wichtige Informationen oder Fortschritte während der Ausführung eines Skripts zu informieren.

## Wie es funktioniert

Das Schreiben auf die Standardfehlerausgabe ist in Bash recht einfach. Man muss einfach den Befehl `echo` followed by `>&2` verwenden, um den Output auf den Standardfehler zu leiten. Hier ist ein Beispiel:

```Bash
# Beispiel Bash-Skript:

#!/bin/bash

echo "Das ist ein Beispiel für die Standardausgabe." # Dieser Output geht auf die Standardausgabe.
echo "Dies ist ein Beispiel für den Standardfehler." >&2 # Dieser Output geht auf die Standardfehlerausgabe.
```

Die Ausführung dieses Skripts würde folgendes Ergebnis zeigen:

```
Das ist ein Beispiel für die Standardausgabe.
Dies ist ein Beispiel für den Standardfehler.
```

Man kann auch Variablen in die Standardfehlerausgabe schreiben, indem man das `>&2` an den entsprechenden Stellen im Skript platziert.

## Tiefere Einblicke

Es gibt verschiedene Möglichkeiten, auf die Standardfehlerausgabe zu schreiben. Eine Möglichkeit ist die Verwendung des `error` Befehls in Bash, der speziell für diesen Zweck entwickelt wurde. Dieser Befehl erlaubt es auch, den Output in verschiedene Farben zu formatieren, um die Lesbarkeit zu verbessern.

Eine andere Möglichkeit ist die Verwendung der `trap` Funktion, die es einem erlaubt, einen bestimmten Befehl oder ein Skript auszuführen, wenn ein bestimmter Fehler auftritt. Zum Beispiel kann man mit der `trap` Funktion einen benutzerdefinierten Fehlermeldungs-Output auf die Standardfehlerausgabe schreiben, wenn ein Skript aufgrund eines Fehlers abgebrochen wird.

## Siehe auch

- [Bash-Skripte debuggen](https://linuxhandbook.com/debug-bash-scripts/)
- [Was ist die Standardfehlerausgabe in Bash?](https://www.linuxjournal.com/content/understanding-bash-what-new-shell-programmer-needs-know-part-iv)