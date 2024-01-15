---
title:                "Erfassen von Zufallszahlen"
html_title:           "Java: Erfassen von Zufallszahlen"
simple_title:         "Erfassen von Zufallszahlen"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum
Warum generieren wir zufällige Zahlen in unserem Java-Programm? Nun, es gibt viele Gründe dafür. Zum Beispiel können sie in Spielen verwendet werden, um den Spielverlauf zu beeinflussen, oder in wissenschaftlichen Experimenten, um zufällige Bedingungen zu simulieren. Letztendlich können zufällige Zahlen auch dazu dienen, Sicherheit in unseren Anwendungen zu gewährleisten.

## Wie geht das?
Um zufällige Zahlen in Java zu generieren, können wir die Klasse `Random` verwenden. Hier ist ein Beispielcode, der eine zufällige ganze Zahl zwischen 0 und 10 ausgibt:

```Java
import java.util.Random;

public class RandomNumberGenerator {

    public static void main(String[] args) {
        // Erstellen einer Instanz der Random Klasse
        Random rand = new Random();
        // Generieren einer zufälligen ganzen Zahl zwischen 0 und 10
        int randomNumber = rand.nextInt(11);
        // Ausgabe der zufälligen Zahl
        System.out.println("Die zufällige Zahl ist: " + randomNumber);
    }
}
```

Wenn wir diesen Code ausführen, erhalten wir eine Ausgabe wie diese:

```
Die zufällige Zahl ist: 8
```

Wir können auch eine zufällige Gleitkommazahl zwischen 0 und 1 erzeugen, indem wir `rand.nextDouble()` verwenden. Wir können auch die Grenzen anpassen, indem wir Argumente an die Methoden `nextInt()` und `nextDouble()` übergeben. Zum Beispiel, `rand.nextInt(50)` würde eine zufällige ganze Zahl zwischen 0 und 49 generieren.

## Tiefgründiges
Die Klasse `Random` basiert auf einem Pseudozufallszahlengenerator (PRNG), der deterministisch ist. Das bedeutet, dass bei jedem Programmstart die gleichen Zahlen generiert werden, es sei denn, wir setzen den Seed-Wert des PRNG mit `rand.setSeed()` explizit. Um eine wirklich zufällige Zahl zu erhalten, müssen wir einen externen Quelle für Entropie verwenden, wie zum Beispiel `SecureRandom` aus dem Paket `java.security`, das auf echten Zufallsereignissen basiert, wie zum Beispiel Mausbewegungen oder Tastatureingaben.

## Siehe auch
- [Oracle Java-Dokumentation: Random](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/util/Random.html)
- [Oracle Java-Dokumentation: SecureRandom](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/security/SecureRandom.html)
- [Java-Tutorial: Generieren von zufälligen Zahlen](https://docs.oracle.com/javase/tutorial/java/data/generateRandom.html)