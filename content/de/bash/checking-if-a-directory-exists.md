---
title:                "Überprüfung ob ein Verzeichnis existiert"
html_title:           "Bash: Überprüfung ob ein Verzeichnis existiert"
simple_title:         "Überprüfung ob ein Verzeichnis existiert"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis existiert, ist ein nützliches Werkzeug für Bash-Programmierer, um sicherzustellen, dass ein bestimmtes Verzeichnis vorhanden ist, bevor sie es verwenden oder darauf zugreifen. Das hilft, Fehler in der Ausführung des Programms zu vermeiden und macht das Skript robuster.

## Wie geht's:
Das Überprüfen eines Verzeichnisses in Bash ist sehr einfach. Verwenden Sie einfach den Befehl `test -d [Verzeichnisname]` oder `[[ -d [Verzeichnisname] ]]`, um zu prüfen, ob das Verzeichnis vorhanden ist. Hier ist ein Beispiel:

```Bash
test -d /home/user/Documents
echo $?
```
Die Ausgabe wird `0` sein, wenn das Verzeichnis vorhanden ist, andernfalls wird sie `1` sein.

## Tiefer eintauchen:
Die Idee, ob ein Verzeichnis vorhanden ist oder nicht, ist seit den Anfängen der Bash-Programmierung wichtig. Früher war es üblich, Verzeichnisnamen hartcodiert in Skripten zu verwenden, was zu fehlerhaften Ausführungen führte, wenn das Verzeichnis nicht vorhanden war. Mit dem Konzept der Verzeichnisüberprüfung können wir sicherstellen, dass das Verzeichnis vor der Verwendung existiert, und gegebenenfalls eine Fehlermeldung ausgeben.

Eine Alternative zur Verwendung des `test`-Befehls ist die Verwendung der `if`-Anweisung, um den Befehl `ls` mit der Option `-d` auszuführen und die Ausgabe zu überprüfen.

Um ein Verzeichnis in Bash anzulegen, verwenden Sie einfach den Befehl `mkdir [Verzeichnisname]`.

## Siehe auch:
- [bash test manual](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- [bash if statement](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Constructs.html#index-if)
- [bash mkdir command](https://www.gnu.org/software/bash/manual/html_node/Creating-Directories.html)