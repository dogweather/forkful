---
title:                "Erzeugung von Zufallszahlen"
html_title:           "Bash: Erzeugung von Zufallszahlen"
simple_title:         "Erzeugung von Zufallszahlen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

Was & Warum? 
Das Generieren von Zufallszahlen ist ein häufig eingesetztes Werkzeug in der Programmierung. Es wird verwendet, um Zufallsereignisse zu simulieren oder um in Spielen oder Verschlüsselungssystemen eine zufällige Komponente hinzuzufügen.

Wie geht's: 
Bash bietet verschiedene Möglichkeiten, um Zufallszahlen zu generieren. Eine Möglichkeit ist die Verwendung des Befehls "shuf", der eine Liste von zufälligen Nummern ausgibt. Beispiel: ```Bash shuf -i 1-10 ``` liefert zehn zufällige Nummern zwischen 1 und 10. 

Eine andere Möglichkeit ist die Verwendung der Variablen $RANDOM, die eine zufällige Zahl zwischen 0 und 32767 zurückgibt. Beispiel: ```Bash echo $RANDOM ``` gibt eine zufällige Zahl aus. 

Tiefer tauchen: 
Die Generierung von Zufallszahlen hat eine lange Geschichte und wird in verschiedenen Bereichen wie Statistik, Kryptographie und Simulationen eingesetzt. Es gibt auch alternative Methoden, um Zufallszahlen zu generieren, wie z.B. die Verwendung von physikalischen Phänomenen wie Rauschen oder Wetterereignissen. 

Weitere Infos: 
https://www.gnu.org/software/bash/manual/html_node/Shell-Arithmetic.html#Shell-Arithmetic 
https://de.wikipedia.org/wiki/Zufallszahlengenerator 
https://www.howtogeek.com/362428/how-to-generate-random-numbers-in-the-linux-shell/