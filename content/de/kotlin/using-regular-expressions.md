---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "Kotlin: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

### Was & Warum?
Reguläre Ausdrücke sind ein nützliches Werkzeug für Programmierer, um Textmuster zu suchen und zu manipulieren. Sie bieten eine effiziente Möglichkeit, komplexe Suchmuster zu definieren, anstatt jede einzelne Zeichenfolge zu überprüfen. Dies kann die Effizienz und Lesbarkeit des Codes verbessern.

### Wie geht's?
In Kotlin sind reguläre Ausdrücke in der Standardbibliothek enthalten und werden durch die Verwendung des ```Regex```-Datentyps unterstützt. Hier ist ein Beispiel dafür, wie Sie mit regulären Ausdrücken in Kotlin arbeiten können:

```
val regex = Regex("Hallo")
val text = "Hallo Welt!"
if (regex.matches(text)) {
    println("Der Text enthält 'Hallo'")
} else {
    println("Der Text enthält nicht 'Hallo'")
}
```

Dieses Beispiel definiert eine Regex-Variablen mit dem Muster "Hallo" und überprüft dann, ob der Text "Hallo Welt!" dieses Muster enthält. Die Ausgabe lautet "Der Text enthält 'Hallo'".

### Tiefes Eintauchen
Reguläre Ausdrücke haben eine lange Geschichte und wurden erstmals in den 1950er Jahren von Stephen Kleene eingeführt. Heutzutage gibt es auch alternative Ansätze wie Parsen oder String-Manipulationsfunktionen, aber reguläre Ausdrücke sind immer noch eine beliebte Wahl für Textmanipulationen. 

Die Implementierung von regulären Ausdrücken in Kotlin basiert auf der Java Regex-Library und bietet ähnliche Funktionen. Eine ausführliche Dokumentation zu regulären Ausdrücken in Kotlin finden Sie in der offiziellen Dokumentation unter [Kotlin Regular Expressions](https://kotlinlang.org/docs/regex.html).

### Sieh auch
- [Kotlin Reference for Regular Expressions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Regular Expressions in Java](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)