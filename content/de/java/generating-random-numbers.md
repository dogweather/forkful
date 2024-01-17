---
title:                "Erzeugen von Zufallszahlen"
html_title:           "Java: Erzeugen von Zufallszahlen"
simple_title:         "Erzeugen von Zufallszahlen"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Was & Warum?
Generieren von Zufallszahlen ist ein häufig genutztes Konzept in der Programmierung, bei dem eine Zufallszahl innerhalb eines bestimmten Bereichs erzeugt wird. Programmierer nutzen dies unter anderem, um Spiele zu erstellen, bestimmte Aktionen zufällig auszuführen und Tests durchzuführen.

# Wie geht's?
Generierung von Zufallszahlen in Java ist einfach und benutzerfreundlich. Mit der ```Random``` Klasse und der ```nextInt()``` Methode können wir eine Zufallszahl innerhalb eines bestimmten Bereichs generieren und diese in einer Variablen speichern.

```java
// Importieren der "Random" Klasse
import java.util.Random;

// Deklaration einer "Random" Instanz
Random random = new Random();

// Generieren einer Zufallszahl zwischen 1 und 10 und Speichern in einer Variablen
int randomNumber = random.nextInt(10) + 1;
```

Die ```nextInt()``` Methode erwartet eine ganze Zahl als Parameter, die als obere Grenze für die Zufallszahl dient. Um eine Zufallszahl zwischen 1 und 10 zu erhalten, haben wir daher den Wert 10 angegeben. Da die generierte Zufallszahl auch 0 sein kann, haben wir noch 1 zu dem Ergebnis addiert.

# Tiefgang
Das Konzept der Zufallszahlen wurde bereits in den 1940er Jahren von Wissenschaftlern entwickelt, um Simulationen in der Physik und dem Militär durchzuführen. Heutzutage gibt es verschiedene Algorithmen zur Generierung von Zufallszahlen, die in der Programmierung eingesetzt werden.

Alternativ zur ```Random``` Klasse gibt es auch die Möglichkeit, die ```Math.random()``` Methode zu nutzen. Diese Methode generiert eine Zufallszahl zwischen 0 und 1 und kann dann entsprechend skaliert werden, um eine Zufallszahl innerhalb eines bestimmten Bereichs zu erhalten.

Die ```Random``` Klasse und die ```Math.random()``` Methode nutzen beide den Java Random Number Generator (RNG) Algorithmus, der auf dem Mersenne-Twister Algorithmus basiert. Dieser Algorithmus gewährleistet eine gleichmäßige Verteilung von Zufallszahlen.

# Siehe auch
Wenn du mehr über die Generierung von Zufallszahlen in Java erfahren möchtest, empfehlen wir dir die offizielle Java Dokumentation zur ```Random``` Klasse und zur ```Math``` Klasse.

Java Random Klasse: https://docs.oracle.com/javase/10/docs/api/java/util/Random.html
Java Math Klasse: https://docs.oracle.com/javase/10/docs/api/java/lang/Math.html