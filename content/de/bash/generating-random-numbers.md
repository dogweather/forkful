---
title:                "Bash: Erstellen von zufälligen Zahlen"
programming_language: "Bash"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von Zufallszahlen ist ein nützliches Werkzeug in der Bash-Programmierung, da es ermöglicht, zufällige Ergebnisse zu erzeugen und somit die Ausführung von Skripten abwechslungsreicher zu gestalten.

## Wie geht man vor

Um Zufallszahlen in Bash zu generieren, können wir die Befehle `shuf` oder `od` verwenden. Mit `shuf` können wir eine Liste von Zahlen mischen und dann die gewünschte Anzahl von zufälligen Elementen ausgeben. Zum Beispiel:

```Bash
$ shuf -i 1-10 -n 3
4
8
2
```
Dies gibt eine zufällige Auswahl von drei Zahlen zwischen 1 und 10 aus.

Eine andere Möglichkeit ist die Verwendung von `od` (octal dump), einem Befehl zum Konvertieren von Daten in eine andere Form, nämlich in Oktalzahlen. Wir können diesen Befehl nutzen, um zufällige Zahlen zu generieren, indem wir die Anzahl der Bits (-N) und den Typ der Ausgabe (-A) festlegen. Zum Beispiel:

```Bash
$ od -An -N4 -i /dev/random
380767032
```
Dies gibt eine zufällige vierstellige Zahl aus.

## Tieferer Einblick

Das Erstellen von Zufallszahlen in Bash basiert auf der Verwendung von Zufallsdaten aus verschiedenen Quellen wie der Prozess-ID, der Systemzeit oder dem Systemzustand. Diese Zufallsdaten werden dann verarbeitet und zu einer zufälligen Zahl vermischt und ausgegeben.

Wenn jedoch eine hohe Qualität der Zufallszahl erforderlich ist, sollte ein anderes Tool wie z.B. `rng-tools` verwendet werden, das Zufallszahlen generiert, indem es Hardware-Entropiequellen wie z.B. die CPU-Rauschkomponente nutzt.

## Siehe auch

- [GNU Coreutils: shuf](https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html)
- [GNU Coreutils: od](https://www.gnu.org/software/coreutils/manual/html_node/od-invocation.html)
- [Linuxize: How to Generate Random Numbers in Bash](https://linuxize.com/post/how-to-generate-random-numbers-in-bash/)