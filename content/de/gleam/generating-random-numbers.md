---
title:    "Gleam: Erzeugen von Zufallszahlen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Situationen, in denen das Generieren von Zufallszahlen in Gleam hilfreich sein kann. Zum Beispiel kann dies für die Erstellung von Testdaten, die Simulation von Szenarien oder die Implementierung von Spielen erforderlich sein. Mit Gleam können Sie einfach und effizient Zufallszahlen generieren, um Ihre Projekte zu verbessern.

## So geht's

Es gibt verschiedene Möglichkeiten, um in Gleam Zufallszahlen zu generieren. Eine Möglichkeit ist die Verwendung der `random`-Bibliothek, die eine Vielzahl von Funktionen zur Erzeugung von Zufallszahlen bietet. Hier ist ein Beispielcode, der mithilfe der `random.float`-Funktion eine Zufallszahl zwischen 0 und 1 generiert und diese dann mithilfe der `io.println`-Funktion ausdruckt:

```
Gleam import random
Gleam import io

let random_num = random.float(0, 1)
io.println(random_num)
```
Die Ausgabe dieses Codes könnte beispielsweise `0.72819` sein. Sie können auch andere Funktionen wie `random.int` verwenden, um ganze Zahlen zu generieren.

Sie können auch benutzerdefinierte Zufallszahlengeneratoren erstellen, indem Sie die `Random`-Struktur aus der `random`-Bibliothek verwenden. Diese Struktur bietet verschiedene Methoden zum Generieren von Zufallszahlen und ermöglicht es Ihnen, Ihren eigenen Algorithmus zu implementieren.

## Tiefer Einblick

Die `random`-Bibliothek verwendet einen Pseudo-Zufallszahlengenerator, der auf dem Mersenne-Twister-Algorithmus basiert. Dieser Algorithmus ist in der Regel schneller und zufälliger als andere Generatoren und wird häufig in anderen Programmiersprachen verwendet.

Beim Generieren von Zufallszahlen ist es wichtig, dass der Seed-Wert, der vom Zufallszahlengenerator verwendet wird, eindeutig ist, damit die Zahlen nicht vorhersagbar sind. In Gleam gibt es eine `random.seed`-Funktion, mit der Sie den Seed-Wert des Generators festlegen können.

## Siehe auch

- Offizielle Gleam-Dokumentation zur Verwendung der `random`-Bibliothek: https://gleam.run/documentation/stdlib/random
- Gleam-Tutorial zur Erstellung eines Zufallszahlengenerators: https://medium.com/@gleamlang/create-your-own-random-number-generator-in-gleam-6a1b7e79f696
- Liste von Mersenne-Twister-Implementierungen in verschiedenen Programmiersprachen: https://en.wikipedia.org/wiki/Mersenne_Twister#Pseudocode_implementation