---
title:                "Generieren von Zufallszahlen"
html_title:           "Javascript: Generieren von Zufallszahlen"
simple_title:         "Generieren von Zufallszahlen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Was ist das und warum machen wir es?

Erzeugung zufälliger Zahlen ist ein wichtiges Konzept in der Programmierung. Es bezieht sich auf die Generierung von Zahlen, die auf keiner bestimmten Reihenfolge oder Vorhersagbarkeit basieren. Programmierer verwenden zufällige Zahlen, um verschiedene Aufgaben zu erfüllen, wie zum Beispiel das Erstellen von einzigartigen Passwörtern, die Steuerung von Computerspielen oder das Durchführen von statistischen Analysen.

# Wie geht das?

Um in Javascript eine zufällige Zahl zu erzeugen, können wir die Funktion ```Math.random()``` verwenden. Diese Funktion gibt eine zufällige Dezimalzahl zwischen 0 und 1 zurück. Um eine ganze Zufallszahl zu erhalten, können wir ```Math.floor()``` verwenden, um die Dezimalzahl auf die nächste ganze Zahl abzurunden.

Beispiel:
```Javascript
let randomNum = Math.floor(Math.random() * 10) + 1;
// gibt eine zufällige Zahl zwischen 1 und 10 zurück
```

# Tiefere Einblicke

Die Verwendung von zufälligen Zahlen hat eine lange Geschichte in der Mathematik und Informatik. Es gibt verschiedene Methoden zur Generierung von Zufallszahlen, wie zum Beispiel die Midpoint-Methode oder die Linear-Kongruenzmethode. 

Eine Alternative zur Verwendung von ```Math.random()``` ist die Verwendung spezialisierter Bibliotheken oder Frameworks, die leistungsfähigere und vielseitigere Funktionen zur Generierung von Zufallszahlen bieten.

Bei der Implementierung von zufälligen Zahlen muss darauf geachtet werden, dass sie wirklich zufällig sind. Eine schlechte Implementierung kann dazu führen, dass die Zahlen nicht richtig verteilt sind und somit vorhersehbar werden.

# Siehe auch

Hier sind einige hilfreiche Quellen, um mehr über die Verwendung von zufälligen Zahlen in Javascript zu erfahren:

- ["Math.random()" Dokumentation von Mozilla Developers Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- ["Generating Random Numbers in JavaScript" von Flavio Copes](https://flaviocopes.com/javascript-random/)
- [Quellcode der Linear-Kongruenzmethode von Wikipedia](https://en.wikipedia.org/wiki/Linear_congruential_generator#Parameters_in_common_use)