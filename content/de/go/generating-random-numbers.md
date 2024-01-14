---
title:                "Go: Erzeugen von Zufallszahlen"
programming_language: "Go"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Randomisierte Zahlen sind in der Programmierung oft unerlässlich. Sie können für Spiele, Simulationen und verschiedene Algorithmen verwendet werden. In diesem Blogbeitrag werden wir uns damit beschäftigen, wie man in Go programmieren kann, um zufällige Zahlen zu generieren.

## Wie geht's

Um zufällige Zahlen in Go zu generieren, verwenden wir die `math/rand` Bibliothek. Zuerst müssen wir einen Zufallszahlengenerator initialisieren, um zufällige Zahlen zu erhalten. Dies können wir mit dem `NewSource()`-Befehl tun, der einen `rand.Source`-Datentyp zurückgibt.

```
rand.Seed(time.Now().UnixNano())

randNum := rand.NewSource(seed)
```

Um eine zufällige Ganzzahl in einem bestimmten Bereich zu generieren, verwenden wir die `Intn()`-Funktion, die eine Ganzzahl zurückgibt, die kleiner als das angegebene Maximum ist.

```
num := randNum.Intn(100)
fmt.Println(num)

// output: 53
```

Für die Generierung von zufälligen Fließkommazahlen verwenden wir die `Float64()`-Funktion, die eine Fließkommazahl zwischen 0.0 und 1.0 zurückgibt.

```
f := randNum.Float64()
fmt.Println(f)

// output: 0.7524236
```

Es ist auch möglich, eine zufällige Zahlenfolge zu erhalten, indem wir die `Zipf()`-Funktion verwenden. Diese Funktion gibt eine Zahl zurück, die einer Zipf-Verteilung folgt.

```
z := randNum.Zipf(2, 10) // Erzeugt eine Zahlenfolge mit exponentiellem Abfall.
fmt.Println(z)

// output: 6
```

## Tiefer Einblick

Um einen genaueren Einblick in die Generierung von Zufallszahlen in Go zu bekommen, können wir uns die Pseudozufallszahlengeneratoren (PRNGs) ansehen. Diese Zahlenfolgen werden durch mathematische Algorithmen generiert und sind nicht wirklich zufällig, aber für die meisten Anwendungen sind sie ausreichend. In Go wird standardmäßig der Mersenne Twister Algorithmus verwendet, um Zufallszahlen zu generieren.

Ein wichtiger Punkt bei der Verwendung von PRNGs ist die Seed-Initialisierung. Wenn wir nicht eine geeignete zeiteffiziente Seed initialisieren, kann dies zu vorhersehbaren Zahlenfolgen führen.

## Siehe auch

- [Die math/rand Bibliothek in der Go-Dokumentation](https://golang.org/pkg/math/rand/)
- [Eine Einführung in die Generierung von Zufallszahlen mit Go](https://tutorialedge.net/golang/golang-random-number-generation-tutorial/)