---
title:                "Kotlin: Unterzeichenketten extrahieren"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstrings ist eine wichtige Fähigkeit für jeden Kotlin-Programmierer. Es ermöglicht uns, spezifische Teile von Zeichenketten zu isolieren und gezielt damit zu arbeiten. In diesem Blog-Beitrag werde ich Ihnen zeigen, wie Sie Teilstrings in Kotlin extrahieren können und warum dies nützlich ist.

## Ausführung

Um einen Teilstring zu extrahieren, können wir die `substring()` Methode verwenden. Diese Methode erwartet zwei Parameter: Den Startindex und den Endindex des Teilstrings, den wir extrahieren möchten.

```Kotlin
val string = "Hallo Welt!"
val substr = string.substring(6, 11)
println(substr) // gibt "Welt" aus
```

Hier haben wir den Teilstring von Index 6 (inklusive) bis Index 11 (exklusive) extrahiert, was dem Wort "Welt" entspricht.

Wir können auch nur einen Startindex angeben, um den Teilstring ab diesem Index bis zum Ende der Zeichenkette zu extrahieren. Oder wir können die Länge des Teilstrings als zweiten Parameter angeben, um einen Teilstring mit einer bestimmten Länge zu erhalten.

```Kotlin
val string = "Hello World!"
val substr1 = string.substring(6) // gibt "World!" aus
val substr2 = string.substring(9, 11) // gibt "ld" aus
```

Es ist auch möglich, eine Methode aufzurufen, die bereits einen Teilstring zurückgibt. Ein Beispiel dafür ist die `replace()` Methode.

```Kotlin
val string = "Hallo Welt!"
val newString = string.replace("Hallo", "Hi")
println(newString) // gibt "Hi Welt!" aus
```

## Tiefergehende Details

Um Substrings zu extrahieren, verwendet Kotlin das Konzept des Indexierens, bei dem jeder einzelne Charakter einer Zeichenkette einer bestimmten Position entspricht. Diese Positionen werden als Indizes bezeichnet und beginnen bei 0.

Um den Teilstring "Welt" aus der Zeichenkette "Hallo Welt!" zu extrahieren, müssen wir also die Indizes 6 bis 11 (exklusive) angeben. Es ist wichtig zu beachten, dass der Endindex nicht zum extrahierten Teilstring gehört und daher exklusive ist.

Die `substring()` Methode gibt immer einen neuen Teilstring zurück, ohne die ursprüngliche Zeichenkette zu verändern. Dies liegt daran, dass Zeichenketten in Kotlin immutabel sind, was bedeutet, dass sie nicht geändert werden können.

## Siehe auch

- [Kotlin Strings - Dokumentation](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin Dev - Beispiele für das Extrahieren von Substrings](https://kotlindev.com/remove-characters-substring-kotlin/)
- [Android Developers Blog - Tipps zur Verwendung von Zeichenketten in Kotlin](https://android-developers.googleblog.com/2019/02/ensure-your-kotlin-code-is-robust-and.html)