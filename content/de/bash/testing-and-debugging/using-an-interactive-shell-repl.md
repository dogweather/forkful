---
date: 2024-01-26 04:11:02.113709-07:00
description: "Wie geht das: In Bash ist Ihr Terminal im Grunde ein REPL. Sie geben\
  \ einen Befehl ein; es liest ihn, wertet ihn aus, gibt das Ergebnis aus und kehrt\u2026"
lastmod: '2024-03-13T22:44:54.061130-06:00'
model: gpt-4-0125-preview
summary: In Bash ist Ihr Terminal im Grunde ein REPL.
title: Nutzung einer interaktiven Shell (REPL)
weight: 34
---

## Wie geht das:
In Bash ist Ihr Terminal im Grunde ein REPL. Sie geben einen Befehl ein; es liest ihn, wertet ihn aus, gibt das Ergebnis aus und kehrt zurück, um auf Ihren nächsten Befehl zu warten. Hier ist ein Beispiel, wie man Bash als REPL verwendet:

```Bash
$ echo "Hallo, Welt!"
Hallo, Welt!
$ x=$((6 * 7))
$ echo $x
42
```

Ihre Eingabe folgt dem `$ ` Prompt, wobei die Ausgabe in der nächsten Zeile gedruckt wird. Einfach, richtig?

## Vertiefende Betrachtung
Bash, kurz für Bourne Again SHell, ist die Standard-Shell auf vielen Unix-basierten Systemen. Es ist ein Upgrade der ursprünglichen Bourne-Shell, die in den späten 1970er Jahren entwickelt wurde. Obwohl Bash ein leistungsfähiges Skriptwerkzeug ist, ermöglicht der interaktive Modus das Ausführen von Befehlen Zeile für Zeile.

Wenn man Alternativen betrachtet, gibt es die Python REPL (einfach `python` in Ihrem Terminal eingeben), Node.js (mit `node`) und IPython, eine erweiterte interaktive Python-Shell. Jede Sprache hat tendenziell ihre eigene REPL-Implementierung.

Unter der Haube sind REPLs Schleifen, die Ihre Eingabe (Befehle oder Code) parsen, ausführen und das Ergebnis an stdout (Ihren Bildschirm) zurückgeben, oft unter direkter Verwendung des Interpreters der Sprache. Diese Unmittelbarkeit des Feedbacks ist hervorragend zum Lernen und für Prototypen.

## Siehe auch
- [Offizielle GNU Bash Dokumentation](https://gnu.org/software/bash/manual/bash.html)
- [Learn Shell Interaktives Tutorial](https://www.learnshell.org/)
- [IPython Offizielle Webseite](https://ipython.org/)
- [REPL.it](https://replit.com/): Ein online REPL für mehrere Sprachen (Nicht nur Bash!)
