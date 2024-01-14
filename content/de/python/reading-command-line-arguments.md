---
title:                "Python: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Die Verwendung von Befehlszeilenargumenten ist ein wichtiges Konzept im Bereich der Python-Programmierung. Durch das Lesen von Befehlszeilenargumenten können wir unserem Programm die Fähigkeit geben, auf externe Eingaben zu reagieren und die Ausführung entsprechend anzupassen. Dies ist besonders nützlich, wenn wir unser Programm an verschiedene Szenarien anpassen müssen oder wenn wir von Benutzern Eingaben erwarten.

## Wie Geht Das

Das Lesen von Befehlszeilenargumenten in Python ist relativ einfach und erfordert nur wenige Zeilen Code. Zuerst müssen wir das `sys`-Modul importieren, um Zugriff auf die Befehlszeilenargumente zu erhalten. Anschließend können wir die Argumente mit der Funktion `sys.argv` auslesen. Hier ist ein Beispielcode:

```Python
import sys

# Lesen der Befehlszeilenargumente
arguments = sys.argv

# Ausgabe des ersten Arguments (Index 0 überspringt den Dateinamen)
print("Das erste Argument ist: " + arguments[1])
```

Wenn wir diesen Code als `command_line_args.py` speichern und von der Befehlszeile aus aufrufen, z.B. mit `python command_line_args.py Hallo Welt`, erhalten wir folgende Ausgabe:

```
Das erste Argument ist: Hallo
```

Wir können auch Optionen mit entsprechenden Argumenten verwenden, indem wir unsere Befehlszeilenargumente als Dictionary lesen. Das folgende Beispiel liest die Option `-n` mit dem dazugehörigen Wert aus:

```Python
import sys

# Lesen der Befehlszeilenargumente als Dictionary
arguments = {}

for arg in sys.argv[1:]:
    # Optionen starten mit einem Bindestrich (-)
    if arg.startswith("-"):
        # Optionen und Werte werden mit einem Leerzeichen getrennt
        name, value = arg[1:].split(" ")
        arguments[name] = value

# Ausgabe des Wertes für die Option "-n"
print("Der eingegebene Name ist: " + arguments["n"])
```

Wenn wir dieses Beispiel als `command_line_args_options.py` speichern und von der Befehlszeile mit `python command_line_args_options.py -n Max` aufrufen, erhalten wir folgende Ausgabe:

```
Der eingegebene Name ist: Max
```

## Tiefer Einblick

Während das Lesen von Befehlszeilenargumenten eine einfache und praktische Methode ist, um unsere Programme an verschiedene Eingaben anzupassen, gibt es noch weitere Möglichkeiten, um die Argumente zu verarbeiten. Wir können zum Beispiel Fehlerbehandlungen hinzufügen, um sicherzustellen, dass die erwarteten Argumente vorhanden sind, oder wir können Module wie `getopt` verwenden, um komplexere Optionen zu verarbeiten.

Insgesamt sollten Befehlszeilenargumente ein wichtiger Teil unserer Python-Programme sein, um sie anpassungsfähiger und benutzerfreundlicher zu machen.

## Siehe Auch

- [Python-Dokumentation zu sys.argv](https://docs.python.org/3/library/sys.html#sys.argv)
- [Weitere Möglichkeiten, Befehlszeilenargumente zu verarbeiten](https://realpython.com/command-line-interfaces-python-argparse/)
- [Tutorial für das Modul getopt](https://www.tutorialspoint.com/python/python_command_line_arguments.htm)