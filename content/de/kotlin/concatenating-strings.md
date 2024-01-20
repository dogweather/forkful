---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Verkettung von Zeichenketten ist der Prozess, mehrere Zeichenketten zu einer zusammenzufügen. Programmierer tun dies, um Text dynamisch zu erstellen oder zu formatieren.

## So geht's:
In Kotlin ist die Verkettung von Zeichenketten unkompliziert. Hier sind einige Beispiele:

```Kotlin
var str1 = "Hallo, "
var str2 = "Welt!"
var result = str1 + str2 // Ergebnis: "Hallo, Welt!"
```

Aber wenn Sie Werte in Ihre Zeichenkette einfügen müssen, dann ist "String Interpolation" vielleicht besser:

```Kotlin
var name = "Welt"
var result = "Hallo, $name!" // Ergebnis: "Hallo, Welt!"
```

## Vertiefung
Traditionell wurde die Verkettung von Zeichenketten in vielen Programmiersprachen durch den "+" Operator erreicht. Aber Kotlin bietet eine alternative Methode namens "String Interpolation".

Die Nutzung "String Interpolation" kann leistungssteigernd sein, wenn viele Verkettungen erfolgen, da es die Notwendigkeit unnötiger Zeichenketten Erzeugung vermindert.

Werfen Sie einen Blick auf diesen Vergleich:

```Kotlin
val list = listOf("Eins", "Zwei", "Drei")
var result = ""

// Traditionelle Methode
for (item in list) {
    result += item
}

// Mit String Interpolation
result = list.joinToString("")
```

Die zweite Methode ist performanter, besonders bei großen Listen oder komplexeren Aufgaben.

## Siehe auch
Für mehr Details:

- Fallstudie zur [Leistungssteigerung durch Verwendung von String Interpolation](https://proandroiddev.com/a-deep-dive-into-kotlin-s-string-templates-and-string-interpolation-5fa9e1df2ead) in Kotlin

Schnappen Sie sich Ihren Kaffee und genießen Sie die Codierung!