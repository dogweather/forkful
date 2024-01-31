---
title:                "Lesen von Kommandozeilenargumenten"
date:                  2024-01-20T17:55:17.336418-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lesen von Kommandozeilenargumenten"

category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?
Kommandozeilenargumente sind Parameter, die einem Bash-Skript übergeben werden, um sein Verhalten dynamisch anzupassen. Programmierer nutzen dies, um Skripte flexibler und wiederverwendbar zu machen – ganz ohne manuelle Änderungen im Skript selbst.

## Wie geht das?
```
#!/bin/bash
# script.sh

echo "Das erste Argument ist: $1"
echo "Das zweite Argument ist: $2"
echo "Alle Argumente: $@"
echo "Anzahl der Argumente: $#"
```
Ausführen des Skripts:
```
$ bash script.sh Apfel Birne
Das erste Argument ist: Apfel
Das zweite Argument ist: Birne
Alle Argumente: Apfel Birne
Anzahl der Argumente: 2
```

## Tiefgang
Die `$1`, `$2`... Variablen sind Positionalparameter, die Bash direkt zur Verfügung stellt. `$@` ist eine spezielle Variable, die alle Argumente als Liste enthält. `$#` gibt die Anzahl der übergebenen Argumente an.

In den frühen Unix-Tagen wurden Skriptargumente für die Anpassung an verschiedene Kontexte und Aufgaben verwendet. Heute sind sie genauso relevant, da sie die Skripte vielseitig und interaktiv machen.

Alternativen umfassen `getopts` und `getopt` für komplexere Szenarien mit benannten Argumenten und Optionen.

Im Kern ermöglicht Bash, dass Argumente unterschiedlichen Positionen zugeordnet und separat oder zusammen abgefragt werden können.

## Siehe auch
- Bash 5.0 Referenzhandbuch: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html
- Advanced Bash-Scripting Guide zu Kommandozeilenargumenten: https://tldp.org/LDP/abs/html/internalvariables.html#ARGLIST
- Ein Artikel über die Verwendung von `getopts`: https://linuxconfig.org/bash-script-getopts-with-command-line-options-arguments
