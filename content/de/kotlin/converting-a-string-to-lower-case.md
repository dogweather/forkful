---
title:                "Umwandeln eines Strings in Kleinschreibung"
html_title:           "Kotlin: Umwandeln eines Strings in Kleinschreibung"
simple_title:         "Umwandeln eines Strings in Kleinschreibung"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum
Manchmal möchten wir bestimmte Operationen auf String-Objekten ausführen, wie zum Beispiel den Vergleich oder die Suche nach bestimmten Zeichenketten. Wenn wir sicherstellen möchten, dass diese Operationen nicht an Groß-/Kleinschreibung scheitern, müssen wir den String in Kleinbuchstaben umwandeln.

## Anleitung
Die Kotlin Standardbibliothek bietet eine einfache Möglichkeit, einen String in Kleinbuchstaben umzuwandeln. Hier ist ein Beispiel:

```Kotlin
val myString = "Hallo WELT!"
val convertedString = myString.toLowerCase()
println(convertedString) // ausgabe: hallo welt!
```

Wir können auch die Erweiterungsfunktion `toLowerCase()` direkt auf einem String-Objekt aufrufen:

```Kotlin
val myString = "Hallo WELT!"
val convertedString = myString.toLowerCase()
println(convertedString) // ausgabe: hallo welt!
```

Die Funktion `toLowerCase()` gibt einen neuen String zurück, der den ursprünglichen String in Kleinbuchstaben enthält. Der ursprüngliche String bleibt unverändert.

## Tieferer Einblick
In Kotlin gibt es verschiedene Methoden, um einen String in Kleinbuchstaben umzuwandeln. `toLowerCase()` ist die grundlegende Methode, die für die meisten Fälle ausreichend ist. Es gibt jedoch noch eine weitere Funktion namens `toLowerCase(Locale)`, die verwendet werden kann, um die Kleinbuchstaben entsprechend der übergebenen Locale-Einstellung zu konvertieren.

```Kotlin
val myString = "Hallo WELT!"
val locale = Locale("de", "DE") // deutsche Locale-Einstellung
val convertedString = myString.toLowerCase(locale)
println(convertedString) // ausgabe: hallo welt!
```

Wenn keine Locale-Einstellung angegeben wird, wird die Standard-Locale des Systems verwendet. Dies kann jedoch zu unerwarteten Ergebnissen führen, wenn der Code auf verschiedenen Systemen ausgeführt wird.

Eine andere Möglichkeit, die Groß-/Kleinschreibung zu ignorieren, ist die Verwendung von `contentEquals()` oder `equalsIgnoreCase()`.

## Weitere Artikel
- [Offizielle Kotlin Dokumentation zu Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin Erweiterungsfunktionen](https://kotlinlang.org/docs/reference/extensions.html)
- [Häufige Fehler beim Umgang mit Strings in Kotlin](https://medium.com/@navdeepsingh_2336/common-mistakes-in-kotlin-with-strings-ce832425defa)