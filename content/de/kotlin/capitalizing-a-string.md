---
title:                "Einen String großschreiben"
html_title:           "Kotlin: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Großschreibung von Zeichenketten in Kotlin

## Was und Warum?
Großschreibung der Zeichenkette (oder das Capitalizing) bedeutet, den ersten Buchstaben jedes Wortes in einer Zeichenkette in Großbuchstaben zu konvertieren. Programmierer machen das oft, um Texte menschenlesbar zu machen, z.B. in Titeln oder am Anfang von Sätzen.

## So geht's:
In Kotlin verwenden wir die Methode `capitalize()` oder `replaceFirstChar()`, um den ersten Buchstaben eines Strings zu großzuschreiben. Hier sind einige Beispiele:

```Kotlin
val kleinerText = "hallo Welt"
println(kleinerText.capitalize()) // Ausgabe: "Hallo Welt"

val meinText = "kotlin macht Spaß"
val meinGroßerText = meinText.replaceFirstChar { if (it.isLowerCase()) it.titlecase(Locale.getDefault()) else it.toString() }
println(meinGroßerText) // Ausgabe: "Kotlin macht Spaß"
```

## Vertiefung
Die Großschreibung von Zeichenketten ist ein gängiges Konzept in vielen Programmiersprachen und gibt es schon seit den frühen Tagen der Informatik. In Kotlin gab es vor der Version 1.5 die Methode `capitalize()`, die jedoch als veraltet gekennzeichnet und durch die Methode `replaceFirstChar()` ersetzt wurde, um mehr Klarheit zu schaffen.

Als Alternativen zur Großschreibung des gesamten Strings können Sie nur den ersten Buchstaben großschreiben oder die Methode `toUpperCase()` verwenden, um jeden Buchstaben des Strings in Großbuchstaben zu konvertieren.

Wenn Sie scharfsinnig auf die Implementierungsdetails achten: `capitalize()` und `replaceFirstChar()` verwenden intern `Char.titlecase()`, was bedeutet, dass sie den Unicode-Standard für die Großschreibung von Zeichen respektieren.

## Siehe auch

Durch den Einsatz sinnvoller Funktionen wie `capitalize()` und `replaceFirstChar()` können Sie die Qualität Ihres Kotlin-Codes erheblich verbessern und sowohl Ihre Arbeitskollegen als auch Ihre Nutzer glücklich machen.