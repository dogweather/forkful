---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum? 

Die Generierung zufälliger Zahlen ist der Prozess, den Programmierer verwenden, um zufällige, nicht vorhersehbare Werte zu erzeugen. Wir machen das, um Fälle von Zufälligkeit in unsere Programme zu implementieren, z. B. ein Würfelspiel oder eine eindeutige Benutzer-ID.

## Wie geht das?

In Arduino könnten Sie die `random()` Funktion nutzen, um Zufallszahlen zu generieren. Hier ist ein einfacher Code-Snippet, der eine Zufallszahl zwischen 0 und 100 ausgibt:

```Arduino
void setup(){
  Serial.begin(9600);
  randomSeed(analogRead(0));
}

void loop(){
  int zufallszahl = random(0, 101);
  Serial.println(zufallszahl);
  delay(1000);
}
```

In diesem Fall, könnte die Ausgabe so aussehen:

```Arduino
45
28
89
12
57
```

## Tiefgreifende Informationen

Die Generierung zufälliger Zahlen hat eine lange Geschichte in der Programmierung. Ursprünglich wurden sie mit speziellen Algorithmen erzeugt, die sogenannte Pseudozufallszahlen erzeugen - sie sehen zufällig aus, folgen aber tatsächlich einem vorbestimmten Muster. 

In der Arduino-Umgebung können Sie die `random()` Funktion oder die `randomSeed()` Funktion verwenden, um Zufallszahlen zu erzeugen. Die `randomSeed()` Funktion wird typischerweise verwendet, um den Ausgangspunkt des Zufallszahlengenerators zu initialisieren und so jede Sequenz von Zahlen einzigartig zu machen.

Alternativ könnten Sie eine externe Bibliothek wie `Entropy` verwenden, die Zufallszahlen durch Hardware-Rauschen erzeugt.

## Siehe auch

Für weitere Informationen über die Generierung von Zufallszahlen, können Sie die folgenden Ressourcen besuchen:

- Arduino Reference: [Random Numbers](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- Arduino Forum: [Best Practices for Random Numbers](https://forum.arduino.cc/index.php?topic=503368.0)
- Arduino Project Hub: [Random Number Generator](https://create.arduino.cc/projecthub/projects/tags/random%20number%20generator)