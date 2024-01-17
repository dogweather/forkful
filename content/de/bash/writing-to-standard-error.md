---
title:                "Schreiben zum Standardfehler"
html_title:           "Bash: Schreiben zum Standardfehler"
simple_title:         "Schreiben zum Standardfehler"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben von Fehlern auf den Standardfehler ist eine Möglichkeit für Programmierer, Fehler und Probleme in ihrem Code anzuzeigen. Es ist eine gängige Praxis, die es Programmierern ermöglicht, Fehler zu erkennen und zu beheben, um sicherzustellen, dass ihr Code richtig funktioniert.

## Wie geht es?

Um Fehler auf den Standardfehler zu schreiben, kann das "echo" Kommando verwendet werden. Zum Beispiel:

```Bash 
echo "Fehler: Datei nicht gefunden" 1>&2 
```

Dies wird den Fehler "Datei nicht gefunden" auf den Standardfehler ausgeben, anstatt auf den Standardausgang.

## Tiefes Eintauchen

Das Schreiben von Fehlern auf den Standardfehler hat eine lange Geschichte in der Welt der Programmierung. Frühere Programmiersprachen hatten möglicherweise keine Möglichkeit, Fehler anzuzeigen, was zu einer Vielzahl von Fehlern und ineffizientem Code führte. Eine Alternative zum Schreiben auf den Standardfehler ist die Verwendung eines Protokollierungstools wie "syslog", um Fehler in einem System zu verfolgen.

Die Implementierung des Schreibens auf den Standardfehler in Bash ist relativ einfach und erfordert nur die Verwendung der "echo" und "1>&2" Befehle, um den Fehler auf den Standardfehler auszugeben.

## Siehe auch

Weitere Informationen zum Schreiben von Fehler auf den Standardfehler finden Sie in der offiziellen [Bash-Dokumentation](https://www.gnu.org/software/bash/manual/html_node/Bourne-Shell-Builtins.html) und in diesen hilfreichen [Tipps für den Umgang mit Fehlern in Bash](https://www.davidpashley.com/articles/writing-robust-shell-scripts/#id2382187).