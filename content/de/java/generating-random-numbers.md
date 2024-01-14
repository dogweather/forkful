---
title:    "Java: Zufällige Zahlen generieren"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Generieren von Zufallszahlen ist ein wichtiger Bestandteil der Programmierung, insbesondere bei Computerspielen oder Simulationen. Mit zufälligen Zahlen können wir ein Element der Unvorhersehbarkeit in unsere Anwendungen integrieren und somit ein realistischeres Nutzererlebnis schaffen.

## How To

Zufallszahlen in Java zu generieren ist sehr einfach. Wir verwenden die Klasse `Random` aus dem Paket `java.util` und rufen die Methode `nextInt()` auf. Dies liefert uns eine zufällige ganze Zahl im Bereich von -2.147.483.648 bis 2.147.483.647 zurück.

``` Java 
import java.util.Random;

// eine new Instant von Random erzeugen
Random random = new Random();

// eine zufällige ganze Zahl generieren und in einer Variable speichern
int randomNum = random.nextInt();

// zufällige ganze Zahl ausgeben
System.out.println(randomNum);
```

Wenn wir eine zufällige Zahl in einem bestimmten Bereich haben möchten, können wir die `nextInt()` Methode mit einem Parameter aufrufen, der den oberen Grenzwert angibt. Zum Beispiel, um eine zufällige Zahl zwischen 0 und 100 zu bekommen, können wir `nextInt(100)` verwenden.

## Deep Dive

Die `Random` Klasse verwendet einen Algorithmus namens "Linear Congruential Generator" zur Generierung von Zufallszahlen. Dieser Algorithmus verwendet eine einfache Berechnung auf der vorherigen Zufallszahl, um eine neue zu generieren. Obwohl dies ausreichend für die meisten Anwendungen ist, gibt es auch andere Verfahren zur Generierung von Zufallszahlen, die eine bessere Verteilung der Zahlen liefern.

Es ist auch wichtig zu beachten, dass sogenannte "zufällige" Zahlen in der Informatik eigentlich Pseudozufallszahlen sind. Das bedeutet, dass sie auf einem deterministischen Verfahren basieren und somit im Grunde genommen vorhersagbar sind. Es gibt jedoch einige Techniken, um die Zyklizität dieser Zahlen zu erhöhen und somit die Vorhersagbarkeit zu verringern.

## Siehe auch

- [Java Random Klasse Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Verschiedene Verfahren zur Generierung von Pseudozufallszahlen](https://de.wikipedia.org/wiki/Zufallszahlengenerator)
- [Zufallszahlengenerierung in der Programmierung](https://www.coursera.org/lecture/java-programming/zufallszahlen-in-der-programmierpraxis-yMsrG)