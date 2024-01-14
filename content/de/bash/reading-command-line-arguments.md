---
title:    "Bash: Kommandozeilenargumente lesen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist eine grundlegende Fähigkeit für die Programmierung in Bash. Es ermöglicht Ihnen, Ihrem Skript Benutzereingaben zu übergeben und es somit vielseitiger und interaktiver zu gestalten.

## Wie geht das?

Um Befehlszeilenargumente in Bash zu lesen, können Sie die vordefinierten Variablen `$1`, `$2`, `$3`, usw. verwenden. Diese repräsentieren die ersten, zweiten, dritten, usw. Argumente, die an das Skript übergeben werden. Im Folgenden finden Sie ein Beispiel eines Skripts, das das erste Argument ausgibt:

```Bash
#!/bin/bash

echo "Das eingegebene Argument lautet: $1"
```

Wenn Sie dieses Skript mit dem Befehl `./script.sh Hallo` aufrufen, wird es "Das eingegebene Argument lautet: Hallo" ausgeben.

Sie können auch überprüfen, wie viele Argumente an das Skript übergeben wurden, indem Sie die vordefinierte Variable `$#` verwenden. Diese gibt die Anzahl der Argumente zurück. Im folgenden Beispiel wird überprüft, ob mindestens zwei Argumente übergeben wurden:

```Bash
#!/bin/bash

if [ $# -lt 2 ]; then
  echo "Mindestens zwei Argumente erforderlich!"
  exit 1
fi

echo "Der zweite und dritte Parameter lauten: $2 $3"
```

Wenn Sie dieses Skript mit dem Befehl `./script.sh Argument1 Argument2 Argument3` aufrufen, wird es "Der zweite und dritte Parameter lauten: Argument2 Argument3" ausgeben.

## Tiefentauchen

Es gibt noch weitere vordefinierte Variablen, die beim Lesen von Befehlszeilenargumenten hilfreich sein können. Zum Beispiel kann `$0` verwendet werden, um den Namen des Skripts selbst zu erhalten, und `$@` gibt alle Argumente als einzelne Strings zurück.

Sie können auch Optionen und Parameter in Ihren Skripten verarbeiten, indem Sie die `getopt`-Funktion verwenden. Diese ermöglicht es Ihnen, Argumente mit Optionen wie `-h` oder `--help` zu versehen und entsprechend zu handhaben.

## Siehe auch

- [Bash-Referenzhandbuch: Befehlszeilenparameter](https://www.gnu.org/software/bash/manual/html_node/Command_002dLine-Options.html)
- [Gute Praktiken im Bash-Skripting: Befehlszeilenargumente verarbeiten](https://www.shellscript.sh/arguments.html)
- [Video-Tutorial: Lesen von Befehlszeilenargumenten in Bash](https://www.youtube.com/watch?v=KxsiUxZSoTA)