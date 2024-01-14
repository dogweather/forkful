---
title:                "Kotlin: Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

#Warum

Manchmal müssen Sie möglicherweise bestimmte Zeichen aus einem Text entfernen, die einem bestimmten Muster entsprechen. Dies kann hilfreich sein, um unerwünschte Zeichen in einem String zu entfernen oder um einen Text zu bereinigen, bevor er weiterverarbeitet wird.

#Wie löscht man Zeichen, die einem Muster entsprechen

Um Zeichen zu löschen, die einem bestimmten Muster entsprechen, können Sie die `replace()` Funktion in Kotlin verwenden. Diese Funktion nimmt zwei Parameter an: das zu ersetzende Muster und den Ersatztext. Im folgenden Beispiel entfernen wir das Sonderzeichen `@` aus einem String:

```Kotlin
val text = "Hallo@Welt"
val bereinigterText = text.replace("@","")
```

Das Ergebnis ist der String "HalloWelt", in dem das `@` durch einen leeren String ersetzt wurde.

Auch die Verwendung von regulären Ausdrücken ist möglich, um Zeichenmuster zu identifizieren und zu löschen. Die `replace()` Funktion nimmt auch reguläre Ausdrücke als Parameter an. Im folgenden Beispiel entfernen wir alle Zahlen aus einem String:

```Kotlin
val text = "Das ist 123 ein Beispiel 456 Text"
val bereinigterText = text.replace("[0-9]".toRegex(),"")
```

Das Ergebnis ist der String "Das ist ein Beispiel Text", in dem alle Zahlen durch einen leeren String ersetzt wurden.

#Tiefere Einblicke

In Kotlin können Sie auch die `replaceAll()` Funktion verwenden, um ein Zeichenmuster zu identifizieren und zu ersetzen. Diese Funktion nimmt ähnlich wie die `replace()` Funktion zwei Parameter an, jedoch kann sie auch mehrere Vorkommen des Musters ersetzen.

```Kotlin
val text = "Das ist 123 ein Beispiel 456 Text"
val bereinigterText = text.replaceAll("[0-9]+","")
```

Das Ergebnis ist der String "Das ist ein Beispiel Text", in dem alle Zahlen durch einen leeren String ersetzt wurden. Im Vergleich zur `replace()` Funktion, die nur das erste Vorkommen des Musters ersetzt hätte, können mit `replaceAll()` alle Vorkommen ersetzt werden.

Eine weitere nützliche Funktion ist `trim()`, die am Anfang und Ende eines Strings alle Leerzeichen oder ein spezifiziertes Zeichen entfernt. Diese Funktion kann hilfreich sein, wenn Sie zum Beispiel eine E-Mail-Adresse bereinigen möchten, um sie in einem String zu verwenden.

#Siehe auch

- [Kotlin String Dokumentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html)
- [Kotlin Regex Dokumentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/index.html)
- [Kotlin String Funktionen](https://kotlinlang.org/docs/reference/string.html)