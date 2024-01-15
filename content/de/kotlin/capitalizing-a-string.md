---
title:                "Großschreibung eines Strings"
html_title:           "Kotlin: Großschreibung eines Strings"
simple_title:         "Großschreibung eines Strings"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Wenn du schon eine Weile mit Kotlin programmiert hast, hast du wahrscheinlich schon bemerkt, dass Strings in dieser Sprache immer kleingeschrieben sind. Das ist eine gute Sache, denn es hilft dabei, den Code übersichtlich zu halten und potenzielle Fehler zu vermeiden. Aber manchmal möchtest du vielleicht einen String in deinem Code groß schreiben. In diesem Artikel zeigen wir dir, wie du das machen kannst und warum du es vielleicht tun möchtest.

## Wie geht man vor?

Um einen String in Kotlin zu capitalizen, gibt es mehrere Möglichkeiten. Eine davon ist die Verwendung der eingebauten Funktion `toUpperCase()`, die einen String komplett in Großbuchstaben umwandelt. Beispiel:

```Kotlin
val text = "hallo Welt"
val capitalizedText = text.toUpperCase()
println(capitalizedText)
```

Dieses Beispiel gibt "HALLO WELT" aus. Beachte, dass die Variable `text` nicht verändert wurde, da Strings in Kotlin unveränderbar sind. Stattdessen wird ein neuer String `capitalizedText` erstellt und dieser wird ausgegeben. Falls du den ursprünglichen String ändern möchtest, kannst du die Funktion `toUpperCase()` direkt auf den String anwenden, also `text.toUpperCase()`.

Eine andere Möglichkeit ist die Verwendung der Funktion `capitalize()`, die den ersten Buchstaben des Strings in einen Großbuchstaben umwandelt. Beispiel:

```Kotlin
val text = "hallo Welt"
val capitalizedText = text.capitalize()
println(capitalizedText)
```

Dieses Beispiel gibt "Hallo Welt" aus. Beachte, dass hier nur der erste Buchstabe des Strings groß geschrieben wurde. Wenn du möchtest, dass alle Wörter des Strings großgeschrieben werden, musst du zusätzlich die Funktion `split()` verwenden, um den String in mehrere Wörter aufzuteilen, und dann `toUpperCase()` auf jedes dieser Wörter anwenden. Beispiel:

```Kotlin
val text = "hallo Welt"
val words = text.split(" ")
var capitalizedText = ""
for (word in words) {
    capitalizedText += word.toUpperCase() + " "
}
println(capitalizedText.trim())
```

Dieses Beispiel gibt "HALLO WELT" aus.

## Tiefere Einblicke

Jetzt fragst du dich vielleicht, warum du überhaupt einen String capitalizen möchtest. Eine häufige Anwendung dafür ist die Validierung von Benutzereingaben. Wenn du zum Beispiel ein Passwortfeld in deiner App hast, möchtest du vermutlich sicherstellen, dass der Benutzer keine einfache Kombination aus Zahlen und Buchstaben wie "12345" verwendet. Eine Möglichkeit, dies zu tun, wäre zu checken, ob das vom Benutzer eingegebene Passwort mit dem gleichen Passwort, das in deiner Datenbank gespeichert ist, übereinstimmt. Aber was passiert, wenn der Benutzer "hallo123" eingibt? In diesem Fall würden sich die Passwörter nicht matchen, obwohl sie offensichtlich gleich sind. Indem du den eingegebenen String capitalizest, kannst du sicherstellen, dass es zu keinem solchen Fehlmatch kommt.

Eine weitere Anwendungsmöglichkeit ist die Arbeit mit APIs. Oftmals erwarten APIs, dass Daten in einem bestimmten Format übergeben werden, zum Beispiel alle Buchstaben groß geschrieben oder alle Wörter großgeschrieben. Indem du Strings in deinem Code capitalizest, kannst du sicherstellen, dass deine Daten richtig formatiert sind, bevor sie an die API gesendet werden.

## Siehe auch

- [Offizielle Kotlin Dokumentation](https://kotlinlang.org/docs/reference/strings.html)
- [Kotlin Strings Tutorial](https://blog.kotlin-academy.com/find-your-way-in-kotlin-strings-7ee1ace0b78f)
- [Kotlin String Manipulation Methods](https://www.baeldung.com/kotlin/string-manipulation)