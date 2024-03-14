---
date: 2024-01-20 17:56:45.498739-07:00
description: "Das Lesen von Befehlszeilenargumenten erm\xF6glicht es Programmen, beim\
  \ Start Parameter von au\xDFen zu empfangen. Diese Flexibilit\xE4t ist essentiell,\
  \ um\u2026"
lastmod: '2024-03-13T22:44:53.394189-06:00'
model: gpt-4-1106-preview
summary: "Das Lesen von Befehlszeilenargumenten erm\xF6glicht es Programmen, beim\
  \ Start Parameter von au\xDFen zu empfangen. Diese Flexibilit\xE4t ist essentiell,\
  \ um\u2026"
title: Lesen von Kommandozeilenargumenten
---

{{< edit_this_page >}}

## Was & Warum?
Das Lesen von Befehlszeilenargumenten ermöglicht es Programmen, beim Start Parameter von außen zu empfangen. Diese Flexibilität ist essentiell, um Programme anpassbar und für verschiedene Szenarien nützlich zu gestalten.

## Anleitung:
Um Befehlszeilenargumente in Python zu lesen, nutzt du das `sys` Modul. Hier ein simples Beispiel:

```python
import sys

def main():
    # Prüfe, ob Argumente vorhanden sind
    if len(sys.argv) > 1:
        print(f"Hallo, {sys.argv[1]}!")
    else:
        print("Hallo, Welt!")

if __name__ == "__main__":
    main()
```

Wenn du dein Programm mit `python script.py Max` ausführst, ist die Ausgabe:

```
Hallo, Max!
```

## Tiefergehende Informationen:
Das Modul `sys.argv` ist der klassische Weg, Befehlszeilenargumente in Python zu handhaben. Historisch gesehen, ist es Teil von Python seit seinen frühesten Versionen. Eine moderne Alternative ist das `argparse` Modul, das komplexere Parsing-Aufgaben und automatische Hilfe-Nachrichten unterstützt. Für einfache Zwecke genügt jedoch `sys.argv`.

Eine wichtige Sache beim Umgang mit `sys.argv` ist, dass alle Argumente standardmäßig als Strings behandelt werden. Wenn du andere Datentypen brauchst, musst du sie konvertieren.

Hier ist ein Beispiel, wie `argparse` verwendet werden könnte:

```python
import argparse

def parse_arguments():
    parser = argparse.ArgumentParser(description='Sage Hallo.')
    parser.add_argument('-n', '--name', default='Welt', help='Dein Name')
    return parser.parse_args()

def main():
    args = parse_arguments()
    print(f"Hallo, {args.name}!")

if __name__ == "__main__":
    main()
```

Starte dieses Skript mit `python script.py -n Max`, um die Ausgabe:

```
Hallo, Max!
```

zu erhalten.

## Siehe auch:
- Die Python [Dokumentation für das sys Modul](https://docs.python.org/3/library/sys.html)
- Die Python [Dokumentation für das argparse Modul](https://docs.python.org/3/library/argparse.html)
- Ein Tutorial für `argparse` auf [realpython.com](https://realpython.com/command-line-interfaces-python-argparse/)
- Eine Diskussion über die Vor- und Nachteile von `sys.argv` und `argparse` auf [stackoverflow.com](https://stackoverflow.com/questions/1009860/how-to-read-process-command-line-arguments)
