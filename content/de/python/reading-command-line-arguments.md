---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Python: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist eine nützliche Fähigkeit, die es erlaubt, Programme flexibler zu gestalten und auf verschiedene Eingaben zu reagieren. Wenn du Python lernst oder bereits ein erfahrener Python-Entwickler bist, ist es wichtig, auch die Arbeit mit Befehlszeilenparametern zu beherrschen.

## Wie es geht

Die Verarbeitung von Befehlszeilenargumenten in Python ist einfach und kann mit nur wenigen Zeilen Code erledigt werden. Zuerst importiere das `sys` Modul:

```Python
import sys
```

Anschließend greifen wir auf die `argv` Variable des `sys` Moduls zu, die eine Liste mit allen Befehlszeilenargumenten enthält. Das erste Argument in der Liste ist immer der Name des Programms selbst. Um die anderen Argumente auszugeben, können wir eine `for` Schleife verwenden:

```Python
for arg in sys.argv[1:]:
    print(arg)
```

In diesem Beispiel verwenden wir die Slice-Notation `sys.argv[1:]`, um das erste Argument - also den Programmnamen - zu überspringen und nur die restlichen Argumente auszugeben.

Probieren wir es aus! Hier ist ein Beispiel, bei dem wir eine Datei als Argument übergeben und deren Textinhalt ausgeben:

```Python
import sys

if len(sys.argv) != 2:  # Überprüft, ob genau ein Argument übergeben wurde
    print("Bitte geben Sie eine Datei als Argument an!")
    exit()

with open(sys.argv[1], "r") as f:
    for line in f:
        print(line)
```

Wenn wir dieses Skript mit dem Argument `test.txt` aufrufen, wird der Inhalt der Datei `test.txt` ausgegeben.

```Python
python read_args.py test.txt
```

```
Dies ist eine Beispieltextdatei.
Hier befindet sich noch mehr Text.
Und noch ein letzter Satz.
```

## Tiefer Einblick

Mit der `getopt` Bibliothek können auch argumentierte Optionen verarbeitet werden. Dies ist besonders nützlich, wenn man komplexe Programme schreibt, die mehrere Optionen unterstützen sollen. Ein weiteres interessantes Feature ist die Möglichkeit, Argumente mit dem `argparse` Modul zu parsen und direkt als Argumente für Funktionen zu nutzen.

## Siehe auch

- [Python sys Modul Dokumentation](https://docs.python.org/3/library/sys.html)
- [Python getopt Modul Dokumentation](https://docs.python.org/3/library/getopt.html)
- [Python argparse Modul Dokumentation](https://docs.python.org/3/library/argparse.html)