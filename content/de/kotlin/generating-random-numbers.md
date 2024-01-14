---
title:    "Kotlin: Generieren von Zufallszahlen"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Warum

Das Generieren von Zufallszahlen ist ein wichtiges Konzept in der Programmierung. Durch die Verwendung von Zufallszahlen können Programme abwechslungsreichere Ergebnisse liefern und Simulationen erstellen, die der Realität näher kommen.

## So geht's

Das Generieren von Zufallszahlen in Kotlin ist ganz einfach. Zuerst müssen wir die Random-Klasse aus dem Paket "kotlin.random" importieren.

```Kotlin
import kotlin.random.Random
```

Dann können wir mit der Funktion "nextInt" eine Zufallszahl erzeugen, indem wir den gewünschten Zahlenbereich als Parameter übergeben.

```Kotlin
val randomNumber = Random.nextInt(1, 10) // gibt eine Zufallszahl zwischen 1 und 10 zurück
```

Wir können auch eine Liste von Zufallszahlen erstellen, indem wir die Funktion "random" verwenden und die Größe der Liste angeben.

```Kotlin
val randomList = List(5) { Random.nextInt(20) } // erstellt eine Liste mit 5 Zufallszahlen zwischen 0 und 20
```

Hier ist ein Beispiel für die Ausgabe von Zufallszahlen mittels der oben genannten Methoden:

```Kotlin
Ausgabe: 3
        [14, 2, 6, 19, 11]
```

## Tiefergehende Informationen

Die Random-Klasse in Kotlin verwendet einen Pseudozufallszahlengenerator, der eine Folge von scheinbar zufälligen Zahlen generiert. Dieser Generator gibt bei jedem Aufruf die nächste Zahl in der Sequenz zurück. Um die Folge zu ändern, können wir den Startwert des Generators mit der Funktion "setSeed" ändern.

```Kotlin
Random.setSeed(42) // setzt den Startwert des Generators auf 42
```

Es ist auch wichtig zu beachten, dass der erstellte Generator eine bestimmte Anzahl von Zufallszahlen erzeugen kann, bevor er in eine wiederholende Sequenz übergeht. Um zu verhindern, dass dies passiert, sollte der Startwert mit jeder neuen Ausführung des Programms geändert werden.

## Siehe auch

- Offizielle Kotlin-Dokumentation zur Random-Klasse: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/
- Artikel über die Verwendung von Zufallszahlen in der Programmierung: https://blog.bitsrc.io/an-introduction-to-random-number-generators-in-javascript-717ccb4343b8
- Quellcode des "Flipping Coin"-Beispiels: https://github.com/kotlin-examples/random-numbers-flipping-coin