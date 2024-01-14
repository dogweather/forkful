---
title:                "Bash: Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

In der Bash-Programmierung ist es häufig erforderlich, zu überprüfen, ob ein bestimmtes Verzeichnis vorhanden ist, bevor bestimmte Aktionen ausgeführt werden. Dies kann dazu beitragen, Fehler zu vermeiden und das Skript robust zu machen. In diesem Blog-Beitrag werden wir lernen, wie man überprüft, ob ein Verzeichnis vorhanden ist und einige weitere nützliche Informationen zu diesem Thema erfahren.

## Wie geht das?

Das Überprüfen, ob ein Verzeichnis vorhanden ist, wird in der Bash-Programmierung mit dem Befehl `test` und dem Flag `-d` durchgeführt. Hier ist ein Beispielcode, der überprüft, ob das Verzeichnis "Dokumente" im aktuellen Verzeichnis vorhanden ist:

```Bash
if test -d Dokumente
then
  echo "Das Verzeichnis Dokumente existiert!"
else
  echo "Das Verzeichnis Dokumente existiert nicht."
fi
```

Wenn das Verzeichnis "Dokumente" existiert, wird die Meldung "Das Verzeichnis Dokumente existiert!" ausgegeben. Andernfalls wird die Meldung "Das Verzeichnis Dokumente existiert nicht." angezeigt.

## Tief tauchen

Neben dem Flag `-d` gibt es noch andere nützliche Flags, die beim Überprüfen von Verzeichnissen verwendet werden können. Zum Beispiel kann `-e` verwendet werden, um zu überprüfen, ob das angegebene Verzeichnis oder eine Datei existiert, und `-w` kann verwendet werden, um zu überprüfen, ob das Verzeichnis oder die Datei beschreibbar ist.

Es ist auch möglich, eine bedingte Ausführung basierend auf dem Ergebnis der Überprüfung durchzuführen. Hier ist ein Beispiel, bei dem eine Datei nur erstellt wird, wenn das Verzeichnis "Dokumente" vorhanden ist:

```Bash
if test -d Dokumente
then
  touch Dokumente/NeueDatei.txt
  echo "Die Datei NeueDatei.txt wurde erstellt."
else
  echo "Das Verzeichnis Dokumente existiert nicht."
fi
```

## Siehe auch

- [Bash Guide for Beginners - Test Constructs](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_01.html)
- [Linuxize - Bash IF Statement](https://linuxize.com/post/bash-if-else-statement/)
- [Shell Scripting Tutorial - Conditional Statements](https://www.shellscript.sh/conditional.html)