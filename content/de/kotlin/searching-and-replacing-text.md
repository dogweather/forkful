---
title:                "Suchen und Ersetzen von Text"
html_title:           "Kotlin: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Situationen, in denen man Text in einem Computerprogramm suchen und durch etwas Neues ersetzen muss. Zum Beispiel kann man so schnell und einfach Tippfehler in einer Datei korrigieren oder mehrere Zeilen Code auf einmal ändern. In diesem Artikel werde ich dir zeigen, wie du dies mit Kotlin ganz einfach machen kannst.

## Wie geht das?

Um Text in Kotlin zu suchen und zu ersetzen, musst du die `replace()` Funktion verwenden. Diese Funktion nimmt zwei Parameter an: den zu suchenden Text und den Text, durch den er ersetzt werden soll.

```Kotlin
// Ersetze alle Vorkommen von "Hund" mit "Katze"
val text = "Ich mag Hunde, aber ich liebe Katzen"
val neuerText = text.replace("Hund", "Katze")
```

Das Ergebnis des obigen Codes wird sein: `Ich mag Katzen, aber ich liebe Katzen`.

Aber was ist, wenn du nur bestimmte Vorkommen des Textes ersetzen möchtest? Keine Sorge, Kotlin bietet dir auch hier eine Lösung. Die `replace()` Funktion hat eine optionale dritte Parameter `ignoreCase`, mit der du angeben kannst, ob die Suche nach dem Text Groß- und Kleinschreibung beachten soll.

```Kotlin
// Ersetze "Hund" nur, wenn es großgeschrieben ist
val text = "Ich mag Hunde, aber ich liebe Katzen"
val neuerText = text.replace("Hund", "Katze", true)
```

Das Ergebnis wäre hier `Ich mag Katzen, aber ich liebe Hunde`. Beachte, dass das erste Vorkommen von `Hund` in `Hunde` nicht geändert wurde, da es nicht großgeschrieben war.

## Tiefer Einblick

Es gibt noch weitere Funktionen in Kotlin, mit denen du die Suche und das Ersetzen von Texten optimieren kannst. Zum Beispiel kannst du mit der `replaceFirst()` Funktion nur das erste Vorkommen des zu suchenden Textes ersetzen. Oder du kannst die `replaceAfter()` und `replaceBefore()` Funktionen verwenden, um nur Texte vor oder nach einem bestimmten Punkt zu ersetzen.

Kotlin bietet außerdem mächtige reguläre Ausdrücke, mit denen du noch spezifischere Suchen durchführen kannst. Diese sind besonders nützlich, wenn du komplexe Suchmuster hast, die nicht mit einfachen Texten ersetzt werden können. Du kannst sie in Kombination mit den oben genannten `replace` Funktionen verwenden.

## Siehe auch

- [Kotlin offizielle Dokumentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [10 Kotlin Tipps und Tricks für Anfänger](https://medium.com/better-programming/10-kotlin-tips-and-tricks-for-beginners-bcfb25aec1e2)
- [Reguläre Ausdrücke in Kotlin](https://kotlinlang.org/docs/regexp.html)