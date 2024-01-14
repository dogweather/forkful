---
title:    "Python: Lösung schreiben"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Warum
Warum sollte man überhaupt in der Konsole schreiben? Anders gesagt, warum sollte man überhaupt standard error nutzen? Die Antwort ist einfach: standard error ist ein wichtiger Teil jeder Programmierung auf der Konsole. Es ist der Kanal, über den Fehlermeldungen und andere wichtige Informationen an den Benutzer zurückgegeben werden.

## Wie man es macht
Wir können standard error in Python einfach nutzen, indem wir die `sys` Bibliothek importieren und dann `sys.stderr` als Standardausgabe für unsere Meldungen verwenden. Zum Beispiel:

```Python
import sys 
sys.stderr.write("Fehler: Division durch Null nicht möglich")
```

Dieser Code wird eine Fehlermeldung mit dem angegebenen Text ausgeben. Der Vorteil von `sys.stderr` ist, dass die Meldung deutlich vom restlichen Output des Programms abgegrenzt ist und somit besser auffällt.

## Tiefentauchen
Es gibt noch weitere interessante Möglichkeiten, wie man standard error nutzen kann. Zum Beispiel können wir es verwenden, um bestimmte Ausgaben nur dann anzuzeigen, wenn ein bestimmter Fehler auftritt. Dazu können wir `try` und `except` Blöcke verwenden, um die Ausgabe an `sys.stderr` zu senden, nur wenn ein bestimmter Fehler auftritt.

```Python
try:
    result = 5 / 0
    print(result)
except ZeroDivisionError:
    sys.stderr.write("Achtung: Division durch Null nicht möglich")
```

Auf diese Weise können wir unsere Ausgabe an die verschiedenen Szenarien anpassen und dem Benutzer gezielt Fehlermeldungen zurückgeben.

## Siehe auch
- [sys - Die System-Bibliothek in Python](https://docs.python.org/de/3/library/sys.html)
- [Python Tutorial: Fehlerbehandlung mit try/except](https://www.python-kurs.eu/python3_handling_exceptions.php)
- [Standard Streams in Python](https://www.tutorialspoint.com/python/python_files_io.htm)