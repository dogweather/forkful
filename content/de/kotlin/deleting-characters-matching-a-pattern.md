---
title:                "Kotlin: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann bei der Verarbeitung von Texten oder Strings in einer Anwendung nützlich sein. Dadurch können ungewünschte Zeichen oder Wörter entfernt werden, um eine saubere und genaue Ausgabe zu erhalten.

# Wie man es macht

Um Zeichen zu löschen, die einem bestimmten Muster entsprechen, können Sie die `replace`-Methode in Kotlin verwenden. Diese Methode nimmt zwei Parameter an: das zu ersetzende Muster und der zu verwendende Ersatz. Im folgenden Beispiel haben wir einen String mit Leerzeichen und möchten alle Leerzeichen entfernen.

```Kotlin
val text = "Hallo Welt!"
val newText = text.replace(" ", "")
println(newText)
```

Die Ausgabe des obigen Beispiels wäre "HalloWelt!". Wie Sie sehen, wurden alle Leerzeichen im ursprünglichen String durch einen leeren String ersetzt.

Wir können auch ein Reguläres Ausdrucksmuster verwenden, um gezielt bestimmte Zeichen zu entfernen. Im folgenden Beispiel möchten wir alle Zahlen aus einem String entfernen.

```Kotlin
val text = "Heute ist der 20. Januar."
val newText = text.replace(Regex("[0-9]"), "")
println(newText)
```

Die Ausgabe wäre "Heute ist der . Januar." Da der Reguläre Ausdruck auf alle Ziffern von 0-9 abzielt, wurden sie alle entfernt.

# Tiefergehende Analyse

Beim Arbeiten mit Regulären Ausdrücken gibt es verschiedene Möglichkeiten, um gezielt Zeichen zu entfernen. Die `replace`-Methode ist nur eine davon. Sie können auch die `removeIf`-Methode verwenden, um nach bestimmten Kriterien Zeichen zu entfernen.

Es ist auch möglich, Zeichen in einem String basierend auf ihrer Position zu löschen. Die `removeRange`-Methode erwartet zwei Indizes, die den Bereich angeben, in dem die Zeichen entfernt werden sollen.

Um komplexere Muster zu löschen, können Sie den `replace`-Befehl mit einem Regulären Ausdruck kombinieren und spezifische Zeichen durch Platzhalter ersetzen.

## Siehe auch

- [String.replace () auf dem offiziellen Kotlin-Dokumentationsportal](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Eine Einführung in Reguläre Ausdrücke in Kotlin](https://www.baeldung.com/kotlin/regex)