---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen von Befehlszeilenargumenten ist der Prozess, bei dem Eingabewerte von der Befehlszeile eines Betriebssystems an ein Programm übergeben werden. Programmierer tun dies, um Flexibilität und Anpassungsfähigkeit ihrer Programme zu erhöhen.

## So geht's:

In Python ist das Lesen von Befehlszeilenargumenten sehr einfach mit dem built-in Modul `sys`. Hier ist ein einfaches Beispiel:

```Python
import sys

def main():
    # sys.argv ist die Liste der Befehlszeilenargumente
    print('Anzahl der Argumente:', len(sys.argv), 'Argumente.')
    print('Argument Liste:', str(sys.argv))
    
if __name__ == "__main__":
    main()
```

Wenn Sie dieses Programm mit Argumenten starten, wie folgt:`python myprogram.py arg1 arg2`, ist die Ausgabe:

```Python
Anzahl der Argumente: 3 Argumente.
Argument Liste: ['myprogram.py', 'arg1', 'arg2']
```

## Tiefere Infos:

Eines der ältesten Konzepte in der Computerprogrammierung ist die Verwendung von Befehlszeilenargumenten. Es ermöglichte die Flexibilität und Skalierbarkeit, die für die Interaktion mit Betriebssystemen erforderlich ist. 

Alternativen zum `sys` Modul in Python sind `argparse` und `click`, die bei komplexeren Anforderungen wie der Vereinfachung der Verwendung von Flags oder der Bereitstellung von detaillierteren Hilfeinformationen nützlich sind.

In Bezug auf die Implementierungsdetails, Python's `sys.argv` ist einfach eine Liste in Python, die Zeichenketten enthält. Das erste Element, `sys.argv[0]`, ist immer der Name des Python-Skripts selbst.

## Siehe auch:

1. Python's `argparse` Dokumentation: <https://docs.python.org/3/library/argparse.html>
2. Python's `click` Dokumentation: <https://click.palletsprojects.com/en/7.x/>
3. Python's `sys` Modul Dokumentation: <https://docs.python.org/3/library/sys.html>
4. Mehr Details über Kommandozeilenargumente: <http://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html>