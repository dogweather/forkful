---
title:    "Go: Zufallszahlen generieren"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum
Random Numbers sind ein wichtiger Bestandteil jeder Programmiersprache und können in vielen verschiedenen Anwendungsbereichen eingesetzt werden. Das Generieren von zufälligen Zahlen in Go kann hilfreich sein, um Daten zu simulieren, Verschlüsselungsalgorithmen zu testen oder zufällige Entscheidungen zu treffen. In diesem Blog-Beitrag werden wir uns ansehen, wie man in Go zufällige Zahlen generieren kann und welche Methoden und Funktionen dafür zur Verfügung stehen.

## Wie geht das?
Um zufällige Zahlen in Go zu generieren, können wir die integrierte Pakete "math/rand" und "time" verwenden. Zuerst müssen wir sicherstellen, dass wir den Zufallsgenerator mit einer eindeutigen Seed-Zahl initialisieren, damit er jedes Mal unterschiedliche Zahlen generiert. Dazu können wir die Funktion "Rand.Seed()" aus dem Paket "math/rand" verwenden und ihr eine beliebige Zahl als Argument übergeben.

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Initialisieren des Zufallsgenerators
	rand.Seed(time.Now().UnixNano())

	// Generieren einer zufälligen Ganzzahl zwischen 0 und 100
	num := rand.Intn(101)
	fmt.Println(num)
}
```

Die obige Funktion "Intn(n)" generiert eine zufällige Ganzzahl zwischen 0 und n. Um beispielsweise einen Wert zwischen 50 und 100 zu erhalten, können wir die Funktion "Intn(51) + 50" verwenden.

Das Paket "math/rand" bietet auch verschiedene andere Funktionen und Methoden, um zufällige Zahlen in verschiedenen Formaten zu generieren, wie zum Beispiel "Int", "Float32" oder "ExpFloat64". Es ist immer wichtig, die Dokumentation zu lesen und zu verstehen, um die richtigen Funktionen für Ihre Anforderungen auszuwählen.

## Tiefer eintauchen
Das Paket "math/rand" in Go verwendet den Mersenne-Twister-Algorithmus als Zufallszahlengenerator. Es ist ein beliebtes Verfahren, um qualitativ hochwertige Zufallszahlen zu erzeugen, aber es gibt auch andere Algorithmen wie den XORShift-Algorithmus oder den Lehmer-Generator, die in anderen Programmiersprachen verwendet werden.

Es ist wichtig zu beachten, dass der Mersenne-Twister-Algorithmus eine vorhersehbare Sequenz von Zufallszahlen generieren kann, wenn der Seed-Wert nicht eindeutig ist. Aus diesem Grund ist es wichtig, den Zufallsgenerator mit einem Wert zu initialisieren, der sich ständig ändert, wie zum Beispiel die aktuelle Systemzeit.

## Siehe auch
- Go Dokumentation zu zufälligen Zahlen: https://golang.org/pkg/math/rand/
- Weitere Informationen zum Mersenne-Twister-Algorithmus: https://de.wikipedia.org/wiki/Mersenne-Twister
- Vergleich verschiedener Zufallszahlengeneratoren: https://www.pcg-random.org/