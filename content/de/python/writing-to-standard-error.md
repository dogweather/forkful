---
title:    "Python: Schreiben auf dem Standardfehler"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum 

Das Schreiben in die Standardfehlerausgabe beim Programmieren kann eine nützliche Werkzeug sein, um Fehlermeldungen und andere wichtige Informationen zu erhalten. Es ist oft einfacher und schneller, als diese Informationen in einer separaten Datei zu speichern.

## Wie geht man vor

Um in die Standardfehlerausgabe zu schreiben, verwenden wir die Funktion `sys.stderr.write()`. Diese nimmt eine Zeichenkette als Argument und gibt sie dann in der Fehlerausgabe aus. Hier ist ein Beispiel, wie wir diese Funktion nutzen können:

```Python
import sys

sys.stderr.write("Dies ist eine Fehlermeldung.")
```

Das Ergebnis davon ist:

```
Dies ist eine Fehlermeldung.
```

Wir können auch Variablen oder andere dynamische Inhalte in die Fehlerausgabe schreiben, indem wir zuerst die Zeichenkette mit dem `format()`-Befehl formatieren:

```Python
import sys
variable = "Wichtige Information"

sys.stderr.write("Dies ist eine {0}.".format(variable))
```

Dies würde dann folgendes ausgeben:

```
Dies ist eine Wichtige Information.
```

## Tiefergehende Informationen

Die Standardfehlerausgabe ist besonders hilfreich, wenn es darum geht, Fehlermeldungen während der Ausführung eines Programms zu erhalten. Sie kann auch verwendet werden, um dem Benutzer wichtige Informationen anzuzeigen, ohne dass es nötig ist, eine separate Datei dafür zu erstellen.

Es ist wichtig zu beachten, dass in Python die Standardfehlerausgabe als `sys.stderr` bezeichnet wird und dass sie standardmäßig auf den Bildschirm ausgegeben wird. Sie können diese jedoch auch auf eine Datei umleiten, wenn Sie es vorziehen, Ihre Fehlermeldungen dort zu speichern. Dies kann erreicht werden, indem Sie den `sys.stderr`-Standardstrom auf eine Datei umleiten:

```Python
import sys

sys.stderr = open("fehler.txt", "w")

sys.stderr.write("Alle Fehler werden nun in dieser Datei gespeichert.")
```

Hier wird die Standardfehlerausgabe in die Datei "fehler.txt" umgeleitet und alle zukünftigen Fehlermeldungen werden in dieser Datei gespeichert.

## Siehe auch

Für weitere Informationen über das Schreiben in die Standardfehlerausgabe in Python, schauen Sie sich diese Quellen an:

- [Python Dokumentation zu sys.stderr](https://docs.python.org/de/3/library/sys.html#sys.stderr)
- [Tutorial zu Standard Streams in Python](https://www.python-course.eu/sys_module.php)
- [Beispielprojekt auf GitHub, das die Standardfehlerausgabe nutzt](https://github.com/jdoe/tutorial-project)