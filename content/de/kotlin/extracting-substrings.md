---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Extrahieren von Teilstrings ist die Methode, Ausschnitte aus einem größeren String zu entnehmen. Programmierer tun dies, um spezifische Informationen aus umfangreichen Textdaten abzurufen oder zu verarbeiten.

## So geht's:

In Kotlin kann ein Teilstring durch verschiedene Methoden extrahiert werden. Dazu gehören `substring()`, `subSequence()`, etc.

```Kotlin
val str = "Hallo Welt!"
println(str.substring(6, 11)) // gibt "Welt" aus

// oder

val seq = str.subSequence(6, 11) // erzeugt eine CharSequence "Welt"
println(seq)
```

Die `substring()` Funktion in Kotlin extrahiert einen Teilstring ab einem gegebenen Startindex bis (aber nicht einschließlich) einem Endindex.

## Vertiefung

Historischer Kontext: Das Bedürfnis, Teilstrings zu extrahieren, existiert seit den frühen Tagen der Computerprogrammierung. Es wurde ursprünglich verwendet, um Textdateien zu analysieren oder Dateien mit bestimmten Schlüsselwörtern zu durchsuchen.

Alternativen: Es gibt viele andere Methoden, um Teilstrings aus einer Zeichenkette in Kotlin zu extrahieren, z.B. `removeRange()` oder `replaceRange()`.

Implementationsdetails: Bei der Extrahierung von Teilstrings in Kotlin ist zu beachten, dass die Indizes 0-basiert sind. Im Beispiel oben beginnt die Zeichenkette "Hallo Welt!" beim Index 0 mit "H" und endet beim Index 10 mit "!". Daher extrahiert `substring(6, 11)` den Teilstring "Welt".

## Siehe auch

Weitere Informationen und Beispiele zur Extrahierung von Teilstrings in Kotlin finden Sie unter den folgenden Links:

1. [Offizielle Kotlin-Dokumentation für substring()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
2. [Offizielle Kotlin-Dokumentation für subSequence()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/sub-sequence.html)
3. [Beispiel zur Verwendung von replaceRange() in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace-range.html)