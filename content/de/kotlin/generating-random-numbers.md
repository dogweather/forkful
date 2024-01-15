---
title:                "Reply toZufallsgenerierung von Zahlen"
html_title:           "Kotlin: Reply toZufallsgenerierung von Zahlen"
simple_title:         "Reply toZufallsgenerierung von Zahlen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit dem Generieren von Zufallszahlen beschäftigen? Ganz einfach: Zufallszahlen sind ein grundlegender Bestandteil vieler Programmieraufgaben, sei es bei der Erstellung von Spielen, Simulationen oder Verschlüsselungen. Sie ermöglichen es, komplexe Probleme zu lösen und unvorhersehbares Verhalten zu erzeugen.

## Wie geht man vor?

Um Zufallszahlen in Kotlin zu generieren, gibt es mehrere Möglichkeiten. Eine einfache Methode ist die Verwendung der `Random`-Klasse, die Teil der Standardbibliothek von Kotlin ist. Hier ein Beispiel:

```Kotlin
val random = Random()
val randomNumber = random.nextInt(100) 
```

In diesem Beispiel wird ein `Random`-Objekt erstellt und anschließend die `nextInt()`-Methode aufgerufen, um eine Zufallszahl zwischen 0 und 100 zu generieren. Je nach Anforderung können auch andere Methoden wie `nextLong()` oder `nextDouble()` verwendet werden.

## Tiefere Einblicke

Manchmal ist es erforderlich, spezifische Anforderungen an die Zufallszahlen zu haben, wie zum Beispiel eine gleichmäßige Verteilung oder eine konkrete Anzahl von Nachkommastellen. In solchen Fällen bietet Kotlin mehrere Möglichkeiten, um diese Anforderungen zu erfüllen.

Eine Möglichkeit ist die Verwendung der `Random.nextDouble()`-Methode in Kombination mit der `Math.pow()`-Funktion, um eine Zufallszahl innerhalb eines bestimmten Bereichs zu erhalten. Hier ein Beispiel, bei dem eine Zufallszahl zwischen 1 und 10 mit zwei Nachkommastellen generiert wird:

```Kotlin
val random = Random()
val randomNumber = Math.pow(10.0, 0.0 - 2.0) * random.nextDouble() + 1.0
println("%.2f".format(randomNumber))
```

Eine andere Möglichkeit ist die Verwendung von `ThreadLocalRandom`, das eine höhere Leistung bietet als `Random` und auch in Multi-Threading-Umgebungen sicher ist.

```Kotlin
val randomNumber = ThreadLocalRandom.current().nextInt(1, 10)
```

Es ist auch möglich, benutzerdefinierte Zufallszahlengeneratoren zu erstellen, indem man die `Random`-Klasse erweitert und die entsprechenden Methoden überschreibt.

## Weitere Ressourcen

Für weitere Informationen und Beispiele zum Generieren von Zufallszahlen in Kotlin empfehlen wir folgende Ressourcen:

- [Kotlin Standard Library](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/index.html)
- [Official Kotlin Blog](https://blog.jetbrains.com/kotlin/)
- [Kotlin Programming: The Big Nerd Ranch Guide](https://www.bignerdranch.com/books/kotlin-programming-the-big-nerd-ranch-guide/)

## Siehe auch

Hier sind einige weitere hilfreiche Links rund um das Thema Zufallszahlen in Kotlin:

- [How to Generate Random Numbers in Android Kotlin](https://medium.com/@NikAgrawal/how-to-generate-random-numbers-in-android-kotlin-522c3bc635c8)
- [Kotlin randomDouble() function example](https://www.concretepage.com/kotlin/kotlin-randomdouble-function-example)
- [Creating Random Numbers With Swift](https://www.swiftdevcenter.com/creating-random-numbers-swift/)