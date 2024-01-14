---
title:    "Kotlin: Verwendung von regulären Ausdrücken"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum
Reguläre Ausdrücke sind ein mächtiges Werkzeug, das in der Programmierung oft unterschätzt wird. Sie ermöglichen es uns, Text auf komplexe und flexible Weise zu durchsuchen und zu manipulieren. Durch die Verwendung von regulären Ausdrücken können wir in kürzester Zeit komplexe Aufgaben lösen, die nur mit herkömmlicher String-Manipulation sehr zeitaufwändig wären.

## Wie funktioniert es
Die Verwendung von regulären Ausdrücken in Kotlin ist relativ einfach. Wir verwenden den `Regex`-Klasse, um ein Objekt zu erstellen, das unserem gewünschten Muster entspricht. Dann können wir diese Regel nutzen, um verschiedene Operationen auf Strings auszuführen. Im folgenden Beispiel durchsuchen wir einen String nach allen Vorkommen von Ziffern und geben sie aus:

```Kotlin
val regex = Regex("\\d+")
val string = "Hallo 123 Kotlin"
val result = regex.findAll(string)
for (match in result) {
   println(match.value)
}
// Output: 123
```

Wie Sie sehen können, erstellen wir zuerst ein Regex-Objekt, indem wir ein Muster als Parameter übergeben. In diesem Fall suchen wir nach einer oder mehreren Ziffern. Dann verwenden wir die `findAll`-Methode, um alle Vorkommen dieses Musters in unserem String zu finden und in einer Liste von `MatchResult`-Objekten zurückzugeben. Schließlich können wir diese Liste durchlaufen und den Wert jedes Vorkommens ausgeben.

## Tiefer in die Materie eintauchen
Reguläre Ausdrücke können viel komplexer sein als nur die Suche nach bestimmten Zeichenfolgen. Sie können auch verwendet werden, um Muster zu definieren, und sogar Teile des gefundenen Strings auszuschneiden oder zu ersetzen. Hier sind einige nützliche Funktionen, die in Kotlin verfügbar sind:

- `replace`: Ersetzt alle Vorkommen eines Musters durch eine bestimmte Zeichenfolge.
- `split`: Teilt den String an jedem Vorkommen eines Musters und gibt ein Array von Substrings zurück.
- `matches`: Überprüft, ob der gesamte String dem angegebenen Muster entspricht.
- `replaceFirst`: Ersetzt das erste Vorkommen eines Musters durch eine bestimmte Zeichenfolge.

Es ist auch wichtig zu beachten, dass reguläre Ausdrücke in Kotlin standardmäßig Groß- und Kleinschreibung beachten. Wenn Sie dies nicht wünschen, können Sie den Modifsierer `IGNORE_CASE` verwenden. Weitere Informationen zu regulären Ausdrücken in Kotlin finden Sie in der offiziellen Dokumentation.

## Siehe auch
- Offizielle Kotlin-Dokumentation: https://kotlinlang.org/docs/reference/regular-expressions.html
- Tutorial zu regulären Ausdrücken in Kotlin: https://www.baeldung.com/kotlin-regular-expressions
- Interaktiver Regex-Designer und Tester: https://regex101.com