---
title:                "Generierung von Zufallszahlen"
aliases: - /de/java/generating-random-numbers.md
date:                  2024-01-27T20:33:56.394505-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generierung von Zufallszahlen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Generieren von Zufallszahlen bezieht sich auf die Erzeugung von unvorhersehbaren Sequenzen oder Einzelwerten innerhalb eines definierten Bereichs. Programmierer verwenden diese Technik aus verschiedenen Gründen, einschließlich Simulationen, Spielen, Sicherheitsanwendungen und Stichprobenmethoden, um Algorithmen unter verschiedenen Bedingungen zu testen.

## Wie:

In Java kann das Generieren von Zufallszahlen mithilfe der `Random`-Klasse aus dem `java.util`-Paket oder den Klassen `ThreadLocalRandom` und `SecureRandom` für spezifische Anwendungsfälle erreicht werden. Die folgenden Beispiele veranschaulichen, wie man diese Klassen verwendet.

### Verwendung der `Random`-Klasse
Die `Random`-Klasse bietet eine Möglichkeit, einfache pseudozufällige Zahlen zu generieren.

```Java
import java.util.Random;

public class RandomExample {
    public static void main(String[] args) {
        Random rand = new Random(); // Erstellt ein Random-Objekt

        int randInt = rand.nextInt(50); // Generiert eine zufällige Ganzzahl von 0 bis 49
        double randDouble = rand.nextDouble(); // Generiert eine zufällige Gleitkommazahl zwischen 0,0 und 1,0
        boolean randBoolean = rand.nextBoolean(); // Generiert ein zufälliges Boolesches
        
        System.out.println("Random Int: " + randInt);
        System.out.println("Random Double: " + randDouble);
        System.out.println("Random Boolean: " + randBoolean);
    }
}
```

### Verwendung der `ThreadLocalRandom`-Klasse
Für nebenläufige Anwendungen ist `ThreadLocalRandom` effizienter als `Random`.

```Java
import java.util.concurrent.ThreadLocalRandom;

public class ThreadLocalRandomExample {
    public static void main(String[] args) {
        int randInt = ThreadLocalRandom.current().nextInt(1, 101); // Von 1 bis 100
        double randDouble = ThreadLocalRandom.current().nextDouble(1.0, 10.0); // Von 1,0 bis 10,0
        
        System.out.println("Random Int: " + randInt);
        System.out.println("Random Double: " + randDouble);
    }
}
```

### Verwendung der `SecureRandom`-Klasse
Für kryptografische Operationen bietet `SecureRandom` ein höheres Maß an Sicherheit.

```Java
import java.security.SecureRandom;

public class SecureRandomExample {
    public static void main(String[] args) {
        SecureRandom secRand = new SecureRandom();
        
        byte[] bytes = new byte[20];
        secRand.nextBytes(bytes); // Füllt Bytes mit sicheren Zufallszahlen
        
        System.out.println("Sichere Zufallsbytes:");
        for (byte b : bytes) {
            System.out.printf("%02x ", b);
        }
    }
}
```

## Tieferer Einblick

Die Erzeugung von Zufallszahlen hat sich seit den Anfängen der Informatik erheblich weiterentwickelt. Javas `Random`-Klasse verwendet eine lineare kongruenzielle Formel zur Generierung von pseudozufälligen Zahlen, die deterministisch und nicht für Hochsicherheitsanwendungen geeignet sind. Dies führte zur Einführung von `SecureRandom`, das fortgeschrittenere Algorithmen (z. B. SHA1PRNG) verwendet, um kryptografisch starke Zufallszahlen zu produzieren.

Dennoch haben `Random` und `SecureRandom` ihre Mängel, wie Leistungsverschlechterungen in mehrfädigen Umgebungen. Die Klasse `ThreadLocalRandom` wurde in Java 7 eingeführt, um dieses Problem zu adressieren, indem sie thread-lokale Zufallszahlengeneratoren bereitstellt und somit die Leistung in nebenläufigen Anwendungen deutlich verbessert.

Während diese Klassen die meisten Bedürfnisse abdecken, könnten Entwickler für extrem hohe Skalierungen oder spezialisierte Anforderungen zusätzliche Bibliotheken erkunden oder benutzerdefinierte Lösungen entwickeln. Es ist essenziell, den richtigen Ansatz basierend auf den Sicherheitsbedürfnissen und Leistungsanforderungen des Anwendungsfalls zu wählen.
