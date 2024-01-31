---
title:                "Überprüfen, ob ein Verzeichnis existiert"
date:                  2024-01-19
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis existiert, erlaubt es uns, sicherzustellen, dass unsere Skripte nur dann laufen, wenn die benötigten Daten verfügbar sind. Programmierer machen das, um Fehler zu vermeiden und die Robustheit der Skripte zu verbessern.

## So geht's:

```Bash
# Überprüfen, ob ein Verzeichnis existiert
if [ -d "/pfad/zum/verzeichnis" ]; then
  echo "Das Verzeichnis existiert."
else
  echo "Das Verzeichnis existiert nicht."
fi
```

Beispielausgabe:

```Bash
Das Verzeichnis existiert.
```

## Tiefergehend
Früher verwendeten viele Bash-Skripte `test`-Befehle, wie `-d`, um Datei-Eigenschaften zu prüfen. Mit der Zeit wurde der `[ ... ]`-Syntax eingeführt, der lesbarer ist. Alternativ hätte man `[[ ... ]]` für erweiterten Test nutzen können, der aber in älteren oder anderen Shells nicht garantiert vorhanden ist. Bei der Implementierung ist zu beachten, dass das `-d` Flag spezifisch für Verzeichnisse ist und nicht prüft, ob ein allgemeiner Pfad existiert – für Dateien gibt es andere Flags wie `-f`.

## Siehe auch:

- Advanced Bash-Scripting Guide: [https://www.tldp.org/LDP/abs/html/](https://www.tldp.org/LDP/abs/html/)
- Bash Conditional Expressions (Man Page): [https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- StackOverflow Diskussionen über Bash-Skripting-Probleme und -Lösungen: [https://stackoverflow.com/questions/tagged/bash](https://stackoverflow.com/questions/tagged/bash)
