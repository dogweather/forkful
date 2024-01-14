---
title:    "Bash: Erzeugung von Zufallszahlen"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Warum

Das Generieren von zufälligen Zahlen ist eine nützliche Fähigkeit in der Bash-Programmierung, die es ermöglicht, zufällige Ergebnisse zu erhalten und somit die Funktionalität von Skripten zu erweitern.

## Wie geht's

Zufällige Nummern in Bash zu generieren ist relativ einfach. Alles, was Sie tun müssen, ist die`$RANDOM`Variable in Ihrem Skript zu verwenden. Diese Variable generiert bei jedem Aufruf eine zufällige Zahl zwischen 0 und 32767. Schauen wir uns ein Beispiel an:

```Bash
#!/bin/bash
echo "Deine zufällige Nummer ist:" $RANDOM 
```

Die Ausgabe könnte beispielsweise wie folgt aussehen:

```
Deine zufällige Nummer ist: 13456
```

Sie können auch einen Bereich für die zufälligen Zahlen festlegen, indem Sie die`$RANDOM`Variable mit dem Modulo-Operator verbinden. Schauen wir uns ein weiteres Beispiel an:

```Bash
#!/bin/bash
echo "Deine zufällige Nummer zwischen 1 und 10 ist:" $((RANDOM%10+1)) 
```

Die Ausgabe könnte zum Beispiel wie folgt lauten:

```
Deine zufällige Nummer zwischen 1 und 10 ist: 7
```

## Eintauchen

Die`$RANDOM`Variable verwendet einen Pseudozufallszahlgenerator, der bei jedem Skriptaufruf dieselben Zahlenfolgen generiert. Wenn Sie also eine wirklich zufällige Nummer benötigen, müssen Sie einen anderen Weg gehen.

Eine Möglichkeit ist die Verwendung des`/dev/random`Gerätedatei, die echte Zufallszahlen mithilfe von Geräuschgeneratoren im Computer generiert. Ein Beispiel für die Verwendung dieser Methode finden Sie unter [diesem Link](https://linuxconfig.org/shell-scripting-tutorial#h1---using-dev-random-reader). Eine weitere Möglichkeit ist die Verwendung des`shuf`Befehls, der in vielen Linux-Distributionen verfügbar ist und auch als Argument eine Datei mit Zahlen akzeptiert, aus der er zufällig auswählen kann.

## Siehe auch

- [Shell-Skripting-Tutorial] (https://linuxconfig.org/shell-scripting-tutorial) 
- [Die Verwendung des Befehls shuf] (https://linuxconfig.org/shuf-command-in-linux-mint-20-ubuntu-20-04-and-centos-8)