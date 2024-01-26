---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:49:18.704831-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Zufallszahlen erzeugt man, um Daten, Verhalten oder Features in einer Anwendung unvorhersehbar zu machen. Programmierer nutzen dies für Spiele, Simulationen, Tests oder Sicherheitsfeatures, um nur einige Anwendungen zu nennen.

## How to:
Java bietet verschiedene Möglichkeiten, Zufallszahlen zu generieren. Hier ein schneller Überblick:

```Java
import java.util.Random;

public class ZufallsGenerator {
    public static void main(String[] args) {
        // Erstelle ein Random Objekt
        Random random = new Random();
        
        // Generiere eine Zufallszahl zwischen 0 (inklusive) und 100 (exklusive)
        int zufallszahl = random.nextInt(100);
        System.out.println("Zufallszahl: " + zufallszahl);
        
        // Für double-Werte nutzen wir nextDouble()
        double zufallsDouble = random.nextDouble();
        System.out.println("Zufallsdouble: " + zufallsDouble);
    }
}
```

Sample Output:
```
Zufallszahl: 42
Zufallsdouble: 0.8400883796346277
```

## Deep Dive:
Zufallszahlen sind ein fundamentales Konzept in der Informatik, verwurzelt in der Kryptografie und Statistik. Java's `java.util.Random` ist eines der grundlegenden Mittel zur Erzeugung pseudozufälliger Zahlen. Warum pseudo? Weil diese Zahlen anhand eines vorgegebenen Algorithmus generiert werden und somit theoretisch vorherbestimmbar sind.

Alternativ kann `SecureRandom` aus dem `java.security` Paket für kryptografisch stärkere Zufallszahlen verwendet werden. Dies ist wichtig für alles, was mit Sicherheit zu tun hat. Java 7 führte zudem `ThreadLocalRandom` ein – diese Klasse ist für den Gebrauch in Mehrthread-Programmen optimiert.

Wichtig zu wissen: Der Ausgangspunkt (Seed) bestimmt die Abfolge der generierten Zahlen. Gleicher Seed = gleiche Zahlenabfolge. Für echte Zufälligkeit ist es oft besser, den Seed nicht manuell zu setzen.

## See Also:
- Java API Dokumentation für `Random`: https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/util/Random.html
- Java API Dokumentation für `SecureRandom`: https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/security/SecureRandom.html
- Ein tieferer Einblick in Zufälligkeit und Algorithmen: http://www.random.org/randomness/
- Eine Erklärung von `ThreadLocalRandom`: https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/util/concurrent/ThreadLocalRandom.html
