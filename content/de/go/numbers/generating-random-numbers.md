---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:32.605170-07:00
description: "Die Generierung von Zufallszahlen in der Programmierung dreht sich darum,\
  \ eine Zahlenfolge zu erstellen, die nicht besser als durch Zufall vorhergesagt\u2026"
lastmod: '2024-03-11T00:14:27.240729-06:00'
model: gpt-4-0125-preview
summary: "Die Generierung von Zufallszahlen in der Programmierung dreht sich darum,\
  \ eine Zahlenfolge zu erstellen, die nicht besser als durch Zufall vorhergesagt\u2026"
title: Zufallszahlen generieren
---

{{< edit_this_page >}}

## Was & Warum?

Die Generierung von Zufallszahlen in der Programmierung dreht sich darum, eine Zahlenfolge zu erstellen, die nicht besser als durch Zufall vorhergesagt werden kann. Programmierer tun dies aus einer Vielzahl von Gründen, einschließlich Simulationen, Spielen und Sicherheitsanwendungen, wo Unvorhersehbarkeit der Schlüssel zur Funktionalität oder Geheimhaltung ist.

## Wie geht das:

In Go werden Zufallszahlen mit dem `math/rand` Paket für Pseudo-Zufallszahlen oder `crypto/rand` für kryptographisch sichere Pseudo-Zufallszahlen generiert. Lassen Sie uns beide erkunden.

### Verwendung von `math/rand` für Pseudo-Zufallszahlen

Zuerst importieren Sie das `math/rand` Paket und das `time` Paket, um den Generator zu initialisieren. Das Initialisieren stellt sicher, dass Sie bei jedem Lauf eine andere Zahlenfolge erhalten.

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println("Eine zufällige Zahl:", rand.Intn(100)) // Generiert eine Zahl zwischen 0 und 99
}
```

Beispielausgabe: `Eine zufällige Zahl: 42`

### Verwendung von `crypto/rand` für kryptographisch sichere Pseudo-Zufallszahlen

Für sicherheitssensitivere Anwendungen eignet sich das `crypto/rand` Paket, da es Zufallszahlen generiert, die schwer vorherzusagen sind, was sie für kryptographische Operationen geeignet macht.

```go
package main

import (
	"crypto/rand"
	"fmt"
	"math/big"
)

func main() {
	n, _ := rand.Int(rand.Reader, big.NewInt(100))
	fmt.Println("Eine sichere zufällige Zahl:", n)
}
```

Beispielausgabe: `Eine sichere zufällige Zahl: 81`

## Vertiefung

Der Kernunterschied zwischen den Paketen `math/rand` und `crypto/rand` in Go ergibt sich aus ihrer Quelle der Entropie und ihren vorgesehenen Anwendungsfällen. `math/rand` generiert Pseudo-Zufallszahlen basierend auf einem anfänglichen Seed; daher ist die Sequenz deterministisch und kann vorhergesagt werden, wenn der Seed bekannt ist. Das ist geeignet für Szenarien, bei denen hohe Leistung und nicht absolute Unvorhersehbarkeit das Hauptanliegen ist, wie bei Simulationen oder Spielen.

Auf der anderen Seite leitet `crypto/rand` Zufälligkeit vom zugrunde liegenden Betriebssystem ab, was es für kryptografische Anwendungen geeignet macht, bei denen Unvorhersehbarkeit entscheidend ist. Dies geht jedoch auf Kosten der Leistung und Komplexität bei der Handhabung der generierten Zahlen (wie das Umgehen mit dem Typ `*big.Int` für Ganzzahlen).

Historisch gesehen hat die Vorstellung der Erzeugung von Zufallszahlen in Computern immer am Rand der wahren "Zufälligkeit" getanzt, wobei frühe Systeme stark von deterministischen Algorithmen abhängig waren, die Zufälligkeit nachahmten. Mit der Evolution der Computer entwickelten sich auch diese Algorithmen weiter und integrierten ausgefeiltere Quellen der Entropie aus ihrer Umgebung.

Trotz dieser Fortschritte ist die Suche nach perfekter Zufälligkeit in der Informatik an sich paradox, angesichts der deterministischen Natur von Computern selbst. Deshalb sind für die meisten Anwendungen, bei denen Vorhersehbarkeit schädlich wäre, kryptographisch sichere Pseudo-Zufallszahlen aus Quellen wie `crypto/rand` die bessere Alternative, trotz ihres Mehraufwands.

Im Wesentlichen geht Go mit zwei verschiedenen Paketen für die Generierung von Zufallszahlen elegant auf den Kompromiss zwischen Leistung und Sicherheit ein und ermöglicht Entwicklern die Wahl basierend auf ihren spezifischen Bedürfnissen.
