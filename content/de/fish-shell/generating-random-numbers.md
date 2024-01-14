---
title:                "Fish Shell: Erzeugung von Zufallszahlen"
simple_title:         "Erzeugung von Zufallszahlen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit der Generierung von zufälligen Zahlen beschäftigen? Nun, zufällige Zahlen sind in der Programmierung äußerst nützlich, insbesondere wenn es darum geht, Tests durchzuführen oder Daten zu verfälschen. Sie können auch bei der Erstellung von Spielen oder bei der Simulation von Prozessen verwendet werden.

## Wie geht das?

Die Generierung von zufälligen Zahlen ist in Fish Shell überraschend einfach. Hier sind ein paar Beispiele, wie man in Fish Shell zufällige Zahlen generieren kann:

```
# Erzeugt eine zufällige Ganzzahl zwischen 1 und 100
set number (math % (random 100 + 1))

# Erzeugt eine zufällige Gleitkommazahl zwischen 0 und 1
set float (math (random) / (random))

# Erzeugt eine zufällige Zeichenfolge aus alphanumerischen Zeichen
set string (seq 16 | (shuf -r -n 1 . /dev/stdin | tr -dc '[:alnum:]'))
```

Die ```random``` Funktion gibt eine zufällige Zahl zwischen 0 und 1 zurück, die mit der ```math``` Funktion in eine passende Ganzzahl oder Gleitkommazahl umgewandelt werden kann. Für die Erzeugung von zufälligen Zeichenfolgen werden die Befehle ```seq```, ```shuf```und ```tr``` verwendet.

## Tiefere Einblicke

Beim Generieren von zufälligen Zahlen gibt es einige Dinge zu beachten. Zum Beispiel ist es wichtig, eine geeignete Anzahl von Zufallszahlen zu verwenden, um sicherzustellen, dass die Ergebnisse tatsächlich zufällig sind. In Fish Shell wird die Anzahl der Zufallszahlen durch den Parameter ```random``` bestimmt, der standardmäßig auf 32 Bits eingestellt ist.

Eine weitere wichtige Sache ist, dass die Methode der Zufallszahlengenerierung von der verwendeten Hardware abhängig ist und daher die Ergebnisse auf verschiedenen Systemen unterschiedlich sein können. Es gibt auch fortgeschrittenere Techniken für das Generieren von zufälligen Zahlen, wie beispielsweise die Verwendung von externen Quellen für mehr Entropie. Für tiefergehende Informationen empfehlen wir, sich mit dem Thema Zufallszahlengenerierung in Fish Shell genauer zu beschäftigen.

## Siehe auch

- [Fish Shell Referenz zur ```random``` Funktion](https://fishshell.com/docs/current/commands.html#command-random)
- [Cheat Sheet mit nützlichen Befehlen für die Generierung von Zufallszahlen in Fish Shell](https://cheatography.com/akkio-sasaki/cheat-sheets/fish-shell-basics/)
- [Blog-Beitrag von Stack Overflow über die Generierung von zufälligen Zeichenfolgen in Fish Shell](https://stackoverflow.com/questions/39715169/generating-random-string-in-fish-shell)