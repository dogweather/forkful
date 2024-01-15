---
title:                "Schreiben auf Standardfehler"
html_title:           "Python: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man überhaupt die Standardfehlerausgabe in Python nutzen? Nun, es gibt verschiedene Gründe dafür. Zum einen kann es hilfreich sein, um während des Programmierens Fehler oder Warnungen anzuzeigen. Zum anderen kann es auch hilfreich sein, um die Funktionalität des Programms zu testen und zu überprüfen.

## Wie

Der Befehl zum Schreiben in die Standardfehlerausgabe in Python ist "sys.stderr.write()". Um dies zu nutzen, müssen wir zunächst das "sys" Modul importieren. Hier ist ein Beispielcode:

```Python
import sys

sys.stderr.write("Das ist eine Fehlermeldung!")
```

Der Output dieser Codezeile wird folgendermaßen aussehen:

> Das ist eine Fehlermeldung!

Es ist wichtig zu beachten, dass die Standardfehlerausgabe in der Regel nicht auf der Konsole angezeigt wird, sondern in einer speziellen Datei für Fehler und Warnungen. Somit können Sie die Ausgabe später überprüfen, auch wenn Ihr Programm bereits beendet ist.

## Deep Dive

Nun wollen wir uns etwas genauer anschauen, wie das Schreiben in die Standardfehlerausgabe funktioniert. Wie bereits erwähnt, müssen wir dafür das "sys" Modul importieren. Dieses Modul ist Teil der Standardbibliothek von Python und enthält eine Vielzahl von Funktionen und Variablen, die mit dem System und der Ausführung des Codes zusammenhängen.

Die Funktion "sys.stderr.write()" nimmt einen String als Argument und schreibt diesen in die Standardfehlerausgabe. In unserem Beispiel haben wir einfach einen Text als String übergeben, aber Sie können natürlich auch Variablen und andere Werte verwenden. Es ist auch möglich, mehrere Strings hintereinander in die Standardfehlerausgabe zu schreiben, indem Sie die Funktion mehrmals aufrufen.

## Siehe auch

- Offizielle Dokumentation zu sys.stderr: https://docs.python.org/3/library/sys.html#sys.stderr
- Weitere Informationen zu Standardfehlerausgabe: https://www.tutorialspoint.com/python3/python_standard_error.htm