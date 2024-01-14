---
title:    "Kotlin: Die Länge eines Strings finden"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum
Das Finden der Länge einer Zeichenfolge mag eine einfache Aufgabe sein, aber es ist eine grundlegende Fähigkeit in der Programmierung. Es ist wichtig, die Länge einer Zeichenfolge zu kennen, um die Ausgabe zu formatieren oder Bedingungen in Ihrem Code festzulegen. In diesem Blog-Beitrag zeigen wir Ihnen, wie Sie die Länge einer Zeichenfolge in Kotlin finden können.

## Wie man es macht
Kotlin macht es einfach, die Länge einer Zeichenfolge mit der Methode `length` zu finden. Schauen wir uns ein Beispiel an:

```Kotlin
val string = "Hallo Welt!"
println(string.length)
```

Ergebnis:
```
11
```

In diesem Beispiel wurde die `length` Methode auf der Zeichenfolge mit dem Text "Hallo Welt!" aufgerufen und das Ergebnis wurde auf der Konsole ausgegeben. Beachten Sie, dass Leerzeichen und Sonderzeichen auch in die Länge der Zeichenfolge einbezogen werden.

Jetzt fragen Sie sich vielleicht, ob die Länge einer Zeichenfolge immer gleich der Anzahl der darin enthaltenen Zeichen ist. In den meisten Fällen ist das richtig, aber es gibt einige Besonderheiten, auf die Sie achten sollten.

In Kotlin sind verschiedene Codierungen von Zeichenfolgen erlaubt, z.B. UTF-8 oder UTF-16. Dies kann Auswirkungen auf die Anzahl der Bytes haben, die für jedes Zeichen in der Zeichenfolge verwendet werden. Wenn Sie genau wissen möchten, wie die Länge einer Zeichenfolge berechnet wird, sollten Sie einen Blick in die offizielle Dokumentation von Kotlin werfen.

## Tiefer eintauchen
Wussten Sie, dass es auch eine Methode `count` gibt, um die Anzahl eines bestimmten Zeichens in einer Zeichenfolge zu finden? Diese Methode kann nützlich sein, wenn Sie z.B. die Anzahl der Vokale in einem Wort zählen möchten. Hier ist ein Beispiel:

```Kotlin
val word = "Programmierung"
println(word.count { it == 'a' })
```

Ergebnis:
```
2
```

Das `count` Argument nimmt eine Funktion entgegen, die entscheidet, ob ein Zeichen in die Zählung einbezogen werden soll oder nicht. In diesem Fall wurde das Zeichen `'a'` übergeben, was bedeutet, dass nur die Vokale im String gezählt werden.

Wenn Sie noch tiefer in die Materie eintauchen möchten, empfehlen wir Ihnen, sich mit den verschiedenen Codierungen von Zeichenfolgen und deren Auswirkungen auf die Länge auseinanderzusetzen.

## Siehe auch
- Offizielle Kotlin-Dokumentation über die `length` Methode: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/length.html
- Weitere Informationen über die `count` Methode: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/count.html
- Eine praktische Anwendung des Zählens von Vokalen in einer Zeichenfolge: https://www.techiedelight.com/counting-vowels-digits-letters-string-kotlin/