---
title:                "Eine Zeichenkette interpolieren"
html_title:           "Arduino: Eine Zeichenkette interpolieren"
simple_title:         "Eine Zeichenkette interpolieren"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

String-Interpolation bedeutet, variable Werte direkt in String-Literale einzubinden. Es erleichtert das Erstellen und Verwalten von Strings indem es die Benutzung von Operatoren wie + oder .concat() vermiedet.

## So geht's:

In Kotlin erfolgt die String-Interpolation mit dem Dollarzeichen ($). Hier sind ein paar Beispiele:

```kotlin
fun main() {
    val name = "Kotlin"
    println("Hallo $name!") // Ausgabe: Hallo Kotlin!
}
```

Man kann auch Ausdrücke evaluieren, indem man sie in geschweifte Klammern einschließt:

```kotlin
fun main() {
    val language = "Kotlin"
    val version = 1.5
    println("$language hat die Version ${version + 0.1}.") // Ausgabe: Kotlin hat die Version 1.6.
}
```

## Tiefere Einblicke

String-Interpolation ist ein Konzept, das in vielen Sprachen, einschließlich Kotlin und modernem Javascript, existiert. Es hilft Codern, lesbaren und sauberen Code zu schreiben.

Eine Alternative zur String-Interpolation kann die Verwendung von String.format sein, das jedoch weniger intuitiv und anfälliger für Fehler ist.

Die Implementierung in Kotlin erfolgt im Hintergrund durch den StringBuilder, wodurch die Effizienz gegenüber String-Konkatenierung erhöht wird.

## Siehe auch

- Kotlin Dokumentation zu Zeichenketten: https://kotlinlang.org/docs/strings.html
- Kotlin-Grundlagen zur Interpolation: https://kotlinlang.org/docs/basic-types.html
- StackOverflow Antworten zum Thema String-Interpolation: https://stackoverflow.com/questions/42438263/string-interpolation-in-kotlin