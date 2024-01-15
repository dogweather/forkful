---
title:                "Fehlersuchausgabe drucken"
html_title:           "Bash: Fehlersuchausgabe drucken"
simple_title:         "Fehlersuchausgabe drucken"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debug-Ausgaben sind eine wichtige Methode, um Fehler in Bash-Programmen zu finden und zu beheben. Durch das Drucken von Debug-Ausgaben können Sie den Programmablauf verfolgen und eventuelle Probleme schnell identifizieren.

## So geht's

Um Debug-Ausgaben in Ihrem Bash-Code zu drucken, können Sie das `echo`-Befehl verwenden. Hier ist ein Beispiel:

```Bash
#!/bin/bash

VAR="Hallo Welt!"
echo "Die Variable VAR enthält: $VAR"
```

Dieses Beispiel verwendet den `echo`-Befehl, um den Wert der Variablen `VAR` zu drucken. Die Ausgabe sieht dann wie folgt aus:

```
Die Variable VAR enthält: Hallo Welt!
```

Sie können auch den `set -x` Befehl verwenden, um den gesamten Programmablauf mit Debug-Ausgaben anzuzeigen. Verwenden Sie jedoch diese Methode mit Vorsicht, da dies zu einer übermäßigen Ausgabe führen kann.

## Tief einsteigen

Es gibt verschiedene Methoden, um Debug-Ausgaben in Bash-Programmen zu drucken. Eine weitere Möglichkeit ist die Verwendung des `printf`-Befehls. Dieser Befehl bietet mehr Möglichkeiten zur Formatierung der Ausgabe. Hier ist ein Beispiel:

```Bash
#!/bin/bash

VAR="42"
printf "Der Wert der Variablen VAR ist %s\n" "$VAR"
```

In diesem Beispiel wird der Wert der Variablen `VAR` mit dem `%s`-Platzhalter formatiert. Die Ausgabe sieht dann wie folgt aus:

```
Der Wert der Variablen VAR ist 42
```

Eine andere nützliche Methode ist die Verwendung von `die`-Befehl, um Debug-Ausgaben in Kombination mit einer Fehlermeldung zu drucken. Hier ist ein Beispiel:

```Bash
#!/bin/bash

NUM=0
if [ "$NUM" -eq 0 ]; then
    die "NUM darf nicht 0 sein!"
fi
```

Wenn das Skript ausgeführt wird und die Bedingung `"$NUM" -eq 0` erfüllt ist, wird die folgende Ausgabe gedruckt:

```
NUM darf nicht 0 sein!
```

## Siehe auch

- [Bash Debugging Guide (auf Englisch)](https://wiki.bash-hackers.org/scripting/debuggingtips)
- [Einführung in die Bash-Programmierung (auf Deutsch)](https://wiki.ubuntuusers.de/Shell/Bash-Programmierung_Einfuehrung/)
- [Bash-Dokumentation (auf Deutsch)](https://www.gnu.org/software/bash/manual/html_node/index.html)