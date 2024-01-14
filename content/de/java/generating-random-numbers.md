---
title:    "Java: Erzeugung von Zufallszahlen"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Warum

Die Generierung von Zufallszahlen ist ein wichtiges Konzept in der Java-Programmierung. Es ermöglicht es uns, zufällige Werte zu erzeugen, die in vielen Anwendungen nützlich sind, wie z.B. bei der Erstellung von Passwörtern oder in Spielen. In diesem Blog-Beitrag werden wir uns genauer damit beschäftigen, wie man in Java Zufallszahlen generieren kann.

## Wie geht's

Die Generierung von Zufallszahlen in Java kann auf unterschiedliche Weise erfolgen. Hier sind einige Beispiele mit dazugehörigem Code:

### Verwenden der Random-Klasse

```Java
import java.util.Random;

// Erstellen eines Objekts der Random-Klasse
Random rand = new Random();

// Generieren einer zufälligen Ganzzahl zwischen 0 und 10
int num = rand.nextInt(11); 

// Generieren einer zufälligen Gleitkommazahl zwischen 0 und 1
double decimal = rand.nextDouble(); 

// Generieren einer zufälligen Ganzzahl im Bereich von 20 bis 50
int range = rand.nextInt(31) + 20;

// Ausgabe der generierten Werte
System.out.println("Zufällige Ganzzahl: " + num);
System.out.println("Zufällige Gleitkommazahl: " + decimal);
System.out.println("Zufällige Ganzzahl im Bereich von 20 bis 50: " + range);
```

### Verwenden der Math-Klasse

```Java
// Generieren einer zufälligen Ganzzahl zwischen 0 und 10
int num = (int) (Math.random() * 11); 

// Generieren einer zufälligen Gleitkommazahl zwischen 0 und 1
double decimal = Math.random(); 

// Generieren einer zufälligen Ganzzahl im Bereich von 20 bis 50
int range = (int) (Math.random() * 31) + 20;

// Ausgabe der generierten Werte
System.out.println("Zufällige Ganzzahl: " + num);
System.out.println("Zufällige Gleitkommazahl: " + decimal);
System.out.println("Zufällige Ganzzahl im Bereich von 20 bis 50: " + range);
```

### Verwenden der ThreadLocalRandom-Klasse

```Java
import java.util.concurrent.ThreadLocalRandom;

// Generieren einer zufälligen Ganzzahl zwischen 0 und 10
int num = ThreadLocalRandom.current().nextInt(0, 11); 

// Generieren einer zufälligen Gleitkommazahl zwischen 0 und 1
double decimal = ThreadLocalRandom.current().nextDouble(); 

// Generieren einer zufälligen Ganzzahl im Bereich von 20 bis 50
int range = ThreadLocalRandom.current().nextInt(20, 51);

// Ausgabe der generierten Werte
System.out.println("Zufällige Ganzzahl: " + num);
System.out.println("Zufällige Gleitkommazahl: " + decimal);
System.out.println("Zufällige Ganzzahl im Bereich von 20 bis 50: " + range);
```

## Tiefentauchen

Die verschiedenen vorgestellten Methoden zur Generierung von Zufallszahlen haben jeweils ihre Vor- und Nachteile. Die Random- und Math-Klasse nutzen eine sogenannte Pseudozufallszahl-Generator, welcher gleiche Ausgaben für gleiche Startwerte produziert. Daher sollten diese Methoden nicht für sicherheitskritische Anwendungen verwendet werden. Die ThreadLocalRandom-Klasse hingegen ist in Multithread-Projekten nützlich, da sie dafür sorgt, dass jeder Thread seine eigene Zufallszahl bekommt.

## Siehe auch

- [Java-Dokumentation zu Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Java-Dokumentation zu Math](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- [Java-Dokumentation zu ThreadLocalRandom](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadLocalRandom.html)