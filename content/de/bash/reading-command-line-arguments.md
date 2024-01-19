---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Lesen von Befehlszeilenargumenten in Bash 

## Was & Warum?
Das Lesen von Befehlszeilenargumenten ist der Prozess, durch den ein Bash-Skript Eingaben vom Benutzer erhält. Programmierer tun dies, um die Funktionalität ihrer Skripte zu erhöhen und eine interaktive Benutzererfahrung zu ermöglichen.

## Wie:
Um Argumente in einem Bash-Skript zu lesen, verwenden wir spezielle Variablen, die als Positionspunkte bezeichnet werden.
```Bash
#!/bin/bash

# Das erste Argument anzeigen
echo $1 

# Das zweite Argument anzeigen
echo $2
```
Wenn wir dieses Skript nun mit den Argumenten "Hallo" und "Welt" ausführen, erhalten wir:
```Bash
$ ./meinSkript.sh Hallo Welt
Hallo
Welt
```

## Vertiefung
Die Verwendung von Befehlszeilenargumenten reicht bis in die Anfänge der Unix-Shell zurück und ist ein zentraler Bestandteil der Bash-Programmierung. Alternativen dazu sind die Verwendung von Eingaben/Tastatureingaben während der Programmausführung oder die Lektüre aus einer Datei. 

Die Implementierung des Lesens dieser Argumente in Bash ist relativ einfach, da die Positionspunkte (z.B. `$1`, `$2`, ...) direkt auf die entsprechenden Argumente verweisen. Der spezielle Parameter `$#` gibt die Anzahl der gelieferten Argumente zurück, und `$0` bezieht sich oft auf den Namen des Skripts selbst.

## Siehe auch
Für weitere Informationen verweisen wir auf folgende Quellen: 
- [Bash Guide for Beginners: Chapter 3. The Bash environment](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_03_02.html)
- [Linuxize: How to Pass Arguments to a Bash Script](https://linuxize.com/post/how-to-pass-arguments-to-a-bash-script/)
- [Bash Handbook: Command Line Arguments](https://github.com/denysdovhan/bash-handbook/blob/master/README.md#command-line-arguments)