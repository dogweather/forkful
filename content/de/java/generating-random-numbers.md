---
title:                "Java: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

##Warum
Die Generierung von Zufallszahlen ist ein wichtiger Bestandteil der Programmierung. Mit Hilfe von Zufallszahlen können Programme zufällig erscheinende Ergebnisse liefern, die für viele Anwendungen wie z.B. Simulationen, Spiele oder Kryptographie von großer Bedeutung sind.

##Wie man Zufallszahlen in Java generiert
Die Java Standardbibliothek bietet die Klasse "Random" an, die uns die Generierung von Zufallszahlen ermöglicht. Hier ist ein Beispielcode, der eine zufällige Ganzzahl zwischen 1 und 10 generiert:

```Java
Random random = new Random();
int randomNumber = random.nextInt(10) + 1;
System.out.println(randomNumber);
```

Dieser Code erzeugt eine Instanz der Klasse "Random" und verwendet dann die Methode "nextInt (int bound)", um eine Ganzzahl im angegebenen Bereich zu generieren. Beachten Sie, dass die Methode einen Wert zwischen 0 (einschließlich) und dem angegebenen Wert (ausschließlich) zurückgibt, daher wird hier 1 zum Ergebnis addiert. Das Ergebnis wird dann auf der Konsole ausgegeben.

Um eine Zufallszahl mit Dezimalstellen zu generieren, können wir die Methode "nextDouble()" verwenden:

```Java
Random random = new Random();
double randomDecimal = random.nextDouble();
System.out.println(randomDecimal);
```

Diese Methode gibt eine zufällige Dezimalzahl zwischen 0.0 (einschließlich) und 1.0 (ausschließlich) zurück.

##Tief eintauchen
Beim Generieren von Zufallszahlen ist es wichtig, dass die Ergebnisse wirklich zufällig sind. Aber wie generiert Java eigentlich diese Zufallszahlen? Die Klasse "Random" verwendet einen sogenannten Pseudozufallszahlengenerator (Pseudo-Random Number Generator, PRNG) und ein sogenanntes Seed. Der Seed ist ein Startwert, der den PRNG initialisiert. Wenn jeder Seed nur einmal verwendet wird, können wir immer zufällige Ergebnisse erwarten.

Es ist jedoch wichtig zu beachten, dass die generierten Zahlen nicht wirklich zufällig sind. Sie werden durch mathematische Berechnungen basierend auf dem Seed generiert. Wenn wir also einen bestimmten Seed verwenden, werden wir jedes Mal die gleiche Folge von Zufallszahlen erhalten. Daher ist es wichtig, dass wir bei der Verwendung des PRNG darauf achten, dass der Seed immer unterschiedlich ist, um wirklich zufällige Ergebnisse zu erhalten.

Hier ist ein Beispielcode, der ein zufälliges Element aus einer Liste auswählt und mithilfe des aktuellen Datums als Seed verwendet:

```Java
Random random = new Random(System.currentTimeMillis());
String[] colors = {"Rot", "Blau", "Gelb", "Grün", "Orange", "Lila"};
int randomIndex = random.nextInt(colors.length);
System.out.println("Die Farbe des Tages ist " + colors[randomIndex]);
```

In diesem Beispiel wird das aktuelle Datum in Millisekunden als Seed verwendet, was bedeutet, dass der Seed jedes Mal, wenn das Programm ausgeführt wird, unterschiedlich ist. Auf diese Weise erhalten wir jedes Mal eine andere zufällige Farbe aus der Liste.

##Siehe auch
- [Java Dokumentation zu Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Wikipedia Artikel über Pseudozufallszahlengeneratoren](https://de.wikipedia.org/wiki/Pseudozufallszahlengenerator)