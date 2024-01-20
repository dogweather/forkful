---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Generierung von Zufallszahlen ist eine Methode, bei der eine Serie von Zahlen erzeugt wird, die keine erkennbare Muster oder Reihenfolge haben. Programmiere verwenden dies, um Daten zu verschleiern, Simulationen durchzuführen und Spiele zu erstellen.

## So geht's:

In Java können wir mithilfe der `Math.random()` Methode oder der Klasse `java.util.Random` Zufallszahlen generieren. Hier sind ein paar Beispiele:

```Java
// Methode 1: Verwendung von Math.random()
double zufallsZahl = Math.random();
System.out.println("Zufallszahl zw. 0.0 bis 1.0: " + zufallsZahl);

// Methode 2: Verwendung von java.util.Random
Random rand = new Random();
int zufallsZahlInt = rand.nextInt(100); // Zufallszahl zw. 0 bis 99
System.out.println("Zufallszahl zw. 0 bis 99: " + zufallsZahlInt);
```

## Tiefere Einblicke

Die Methode der Zufallszahlgenerierung hat ihre Wurzeln in der Kryptographie und wurde verwendet, um geheime Codes zu erstellen und zu brechen. Es gibt mehrere alternative Methoden zur Zufallszahlgenerierung in Java, einschließlich `java.security.SecureRandom` und `java.util.concurrent.ThreadLocalRandom`. Die Wahl hängt von den spezifischen Anforderungen deines Programms ab, einschließlich Sicherheitsbedarf und Leistung.

Die Implementierungsdetails variieren je nach Methode, aber im Allgemeinen verwenden beide oben diskutierten Methoden einen sogenannten "Seed" (Anfangswert) als Ausgangspunkt. Die Methode `java.util.Random` generiert Pseudo-Zufallszahlen, wobei der Seed auf der Systemzeit basiert.

## Siehe auch:

Um mehr über Zufallszahlen und ihre Anwendungen in Java zu erfahren, empfehle ich die folgenden Quellen:

- [Offizielle Java Doku zufor java.util.Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Offizielle Java Doku für Math.random()](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#random--)