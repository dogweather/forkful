---
title:                "Lesen von Eingabeparametern"
html_title:           "Fish Shell: Lesen von Eingabeparametern"
simple_title:         "Lesen von Eingabeparametern"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen von Befehlszeilenargumenten ist ein grundlegender Teil der Programmierung in Fish Shell. Es ermöglicht Programmierern, Eingaben von Benutzern effizient zu verarbeiten und ihre Programme interaktiver zu gestalten.

## Wie das geht:
Fish Shell bietet eine einfache Syntax zum Lesen von Argumenten aus der Befehlszeile. Hier ist ein Beispiel:

```Fish Shell
set name $argv[1]
echo "Hallo, $name!"
```
Dieser Code liest das erste Argument von der Befehlszeile und weist es der Variable "name" zu. Dann wird der Wert dieser Variable in einer Ausgabe verwendet.

Wenn wir also Folgendes in der Befehlszeile eingeben:

```
fish read-arguments.fish Max
```
erhalten wir die Ausgabe:
```
Hallo, Max!
```

## Tiefere Einblicke:
Das Lesen von Befehlszeilenargumenten ist keine Erfindung von Fish Shell. Es ist ein Konzept, das in vielen anderen Programmiersprachen und Shell-Umgebungen verwendet wird. Einige alternative Methoden zum Lesen von Argumenten sind die Verwendung von Umgebungsvariablen oder die Verwendung des Programmierungsmusters "Option-Argument". Die Implementierung von Argumentenlesung in Fish Shell basiert auf der Verwendung des globalen argv-Arrays.

## Siehe auch:
- [Die offizielle Fish Shell Dokumentation](https://fishshell.com/docs/current/index.html)
- [Ein Tutorial zum Lesen von Argumenten in Fish Shell](https://medium.com/@clauswitt/how-to-get-command-line-arguments-in-fish-shell-730d69c6b7a3)
- [Weitere Informationen zu Alternativen zum Lesen von Argumenten](https://stackoverflow.com/questions/14496251/what-is-the-difference-between-command-line-arguments-and-environment-variables)