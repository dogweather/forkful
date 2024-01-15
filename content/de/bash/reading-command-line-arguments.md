---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Bash: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum
Du fragst dich vielleicht, warum du überhaupt command line arguments lesen solltest. Nun, es gibt einige Vorteile, wenn du lerst, wie du sie richtig nutzt. Zum Beispiel kannst du damit dein Skript interaktiver gestalten und es flexibler machen, da du verschiedene Optionen und Parameter eingeben kannst.

## Wie man command line arguments liest
Die Syntax zum Lesen von command line arguments ist recht einfach, du musst nur folgendes beachten:
```
Bash
$1 $2 $3
```
Diese Variablen repräsentieren die ersten, zweiten und dritten Argumente, die du beim Aufruf des Skripts eingibst. Zum Beispiel:
```
Bash
$ ./script.sh argument1 argument2 argument3

```
In diesem Beispiel würde `$1` "argument1", `$2` "argument2" und `$3` "argument3" sein. Du kannst beliebig viele Argumente eingeben und sie mit ` $4`, `$5`, usw. ansprechen.

## Vertiefung
Wenn du mehr über command line arguments erfahren möchtest, solltest du dich mit der `getopts` Funktion vertraut machen. Damit kannst du Optionen mit dazugehörigen Argumenten angeben, die dein Skript dann auslesen kann. Du kannst auch Flags definieren, die du einfach nur mit `Skriptname -f` eingeben musst. Es gibt viele Möglichkeiten, command line arguments zu lesen und sie zu nutzen, deshalb ist es eine wichtige Fähigkeit für jeden Bash-Programmierer.

## Siehe auch
- [Bash Command Line Arguments Tutorial](https://dev.to/eevajonnapanula/read-command-line-arguments-in-bash-scripts-1615/)
- [Bash scripting cheatsheet](http://cheatsheetworld.com/programming/unix-linux-cheat-sheet/)
- [getopts documentation](https://www.gnu.org/software/bash/manual/html_node/Bourne-Shell-Builtins.html#index-getopts)