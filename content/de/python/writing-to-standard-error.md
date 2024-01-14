---
title:                "Python: Schreiben in Standardfehler"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben in die Standardfehlerausgabe kann sehr hilfreich sein, um Fehlermeldungen und andere wichtige Informationen während der Ausführung eines Programms anzuzeigen. Dadurch können Programmiererinnen und Programmierer mögliche Probleme schnell erkennen und beheben.

## Wie geht das

Um in Python in die Standardfehlerausgabe zu schreiben, können wir die `sys`-Bibliothek verwenden. Wir müssen es importieren und dann die `stderr`-Methode aufrufen, um die schreibgeschützte Standardfehlerausgabe zu erhalten. Dann können wir die `write()`-Methode verwenden, um Text in die Standardfehlerausgabe zu schreiben.

```Python
import sys

sys.stderr.write("Dies ist ein Fehler! Bitte beheben Sie es.")
```

Das obige Beispiel würde folgende Ausgabe erzeugen:

```
Dies ist ein Fehler! Bitte beheben Sie es.
```

## Tiefer gehend

Standardfehlerausgabe wird oft im Zusammenhang mit Standardausgabe (Standard Output) verwendet. Während Standardausgabe für die Anzeige von normalen Programmausgaben verwendet wird, kann Standardfehlerausgabe für die Anzeige von kritischen Fehlern oder Warnungen verwendet werden.

Ein häufiges Problem beim Schreiben in die Standardfehlerausgabe ist, dass es standardmäßig nicht gepuffert wird. Das bedeutet, dass die Ausgabe jedes Mal sofort angezeigt wird, wenn sie geschrieben wird. Wenn Sie jedoch eine große Menge an Ausgabe haben, kann dies zu Unordnung und Unlesbarkeit führen. Um dies zu vermeiden, kann die `flush()`-Funktion verwendet werden, um die Ausgabe zu puffern und sie erst dann anzuzeigen, wenn sie vollständig ist.

## Siehe auch

- [Python-Dokumentation für sys-Bibliothek](https://docs.python.org/3/library/sys.html)
- [Blog-Post über Standardfehlerausgabe in Python](https://realpython.com/python-logging-source-code/#the-standard-error-stream)