---
title:                "Bash: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum 

Das Lesen und Verstehen von Befehlszeilenargumenten ist eine wichtige Fähigkeit für jeden, der sich mit der Bash-Programmierung beschäftigt. Es ermöglicht die Verwendung von Befehlszeilenoptionen, die Programmabläufe steuern und die Effizienz des Codes verbessern können.

## How To 

Befehlszeilenargumente werden durch die Verwendung von speziellen Variablen in der Bash-Skriptdatei aufgenommen. Die gebräuchlichste Variable ist "$1", die das erste Argument nach dem Skriptnamen repräsentiert. Weitere Argumente können durch die Verwendung von "$2", "$3" usw. abgerufen werden. 

Das folgende Beispiel zeigt, wie ein Skript mithilfe von Befehlszeilenargumenten aufgerufen werden kann: 

```Bash
#!/bin/bash 
echo "Das Skript wurde mit folgenden Argumenten aufgerufen:" 
echo "Das erste Argument ist: $1" 
echo "Das zweite Argument ist: $2" 
echo "Das dritte Argument ist: $3" 
```

Wenn dieses Skript als "argumente.sh" gespeichert und mit den Argumenten "Hallo Welt" aufgerufen wird, lautet die Ausgabe: 

```
$ ./argumente.sh Hallo Welt 
Das Skript wurde mit folgenden Argumenten aufgerufen: 
Das erste Argument ist: Hallo 
Das zweite Argument ist: Welt 
Das dritte Argument ist: 
```

Beachten Sie, dass das Leerzeichen als Trennzeichen zwischen den Argumenten verwendet wird. Wenn ein Argument Leerzeichen enthält, muss es in Anführungszeichen gesetzt werden, um als einzelnes Argument erkannt zu werden. 

## Deep Dive 

Es gibt viele nützliche Funktionen und Techniken, die verwendet werden können, um Befehlszeilenargumente in Bash-Skripten zu lesen. Zum Beispiel können die in "$@" gespeicherten Argumente verwendet werden, um alle Argumente als Liste abzurufen. Auch können Bedingungen und Schleifen verwendet werden, um bestimmte Argumente zu überprüfen oder zu durchlaufen.

Es gibt jedoch auch einige Dinge zu beachten beim Lesen von Befehlszeilenargumenten in Bash. Zum Beispiel können leere Argumente oder Sonderzeichen wie "*" Probleme verursachen. Es ist daher wichtig, immer auf eine sichere Eingabebehandlung zu achten und sicherzustellen, dass das Skript nicht anfällig für Sicherheitsrisiken ist.

## Siehe auch 

- [Bash-Referenzhandbuch: Lesen und Verwenden von Befehlszeilenargumenten](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html#Shell-Parameter-Expansion)
- [Bash-Skripte schreiben: Verwendung von Befehlszeilenargumenten](https://www.shellscript.sh/command-line.html)
- [10 praktische Beispiele für die Verwendung von Befehlszeilenargumenten in Bash-Skripten](https://linuxhint.com/argument_handling_bash/)