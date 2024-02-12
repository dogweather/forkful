---
title:                "Anführungszeichen aus einem String entfernen"
aliases:
- de/kotlin/removing-quotes-from-a-string.md
date:                  2024-01-26T03:41:11.128008-07:00
model:                 gpt-4-0125-preview
simple_title:         "Anführungszeichen aus einem String entfernen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Entfernen von Anführungszeichen aus einem String bedeutet, jegliche Instanzen von Anführungszeichen, entweder einzelne (' ') oder doppelte (" "), aus den Textdaten, mit denen Sie arbeiten, zu entfernen. Programmierer müssen dies oft für die Datenbereinigung tun, um sich auf eine weitere Verarbeitung vorzubereiten, oder wenn die Anführungszeichen selbst für die Bedeutung der Daten nicht relevant sind.

## Wie geht das:

Hier ist eine einfache Methode, um beide Arten von Anführungszeichen aus einem String in Kotlin zu entfernen:

```kotlin
fun removeQuotes(input: String): String {
    return input.replace("\"", "").replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rockt\" es ist 'cool'"
    val stringWithoutQuotes = removeQuotes(stringWithQuotes)
    println(stringWithoutQuotes) // Ausgabe: Kotlin rockt es ist cool
}
```

Und wenn Sie nur einen Typ von Anführungszeichen entfernen möchten, lassen Sie einfach den anderen Ersetzungsaufruf aus.

```kotlin
fun removeDoubleQuotes(input: String): String {
    return input.replace("\"", "")
}

fun removeSingleQuotes(input: String): String {
    return input.replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rockt\" es ist 'cool'"
    println(removeDoubleQuotes(stringWithQuotes)) // Ausgabe: Kotlin rockt es ist 'cool'
    println(removeSingleQuotes(stringWithQuotes)) // Ausgabe: Kotlin "rockt" es ist cool
}
```

## Tiefergehend

Historisch gesehen war die Bearbeitung von Strings und das Escapen von Zeichen ein Kernstück der Programmierung, da Texte eine grundlegende Art und Weise sind, wie wir mit Daten interagieren. Anführungszeichen in Strings müssen manchmal escaped werden. Dies wird durch einen vorangestellten Backslash angezeigt (z.B. `"Sie sagte, \"Hi!\""`). Bei der Verarbeitung solcher Strings müssen Sie möglicherweise die Escape-Zeichen oder die Anführungszeichen selbst entfernen, um einen saubereren oder nutzbareren Text zu erhalten.

Alternativen zur `replace`-Methode umfassen das Entfernen basierend auf Regex oder das manuelle Parsen des Strings, Zeichen für Zeichen. Jedoch kann Regex für einfache Operationen übertrieben sein und manuelles Parsen ist weniger effizient als die Verwendung eingebauter String-Funktionen. Die `replace`-Funktion von Kotlin nutzt die darunterliegende `String` `replace`-Methode von Java, die gut für die Leistung optimiert ist.

In Bezug auf die Implementierung ist es erwähnenswert, dass Kotlin mit Java interoperabel ist, sodass jede Operation, die Sie an Strings durchführen, genauso leistungsfähig ist, wie sie in Java wäre. Es ist entscheidend, beim Entfernen von Anführungszeichen auf Randfälle zu achten, wie verschachtelte Anführungszeichen, die einen ausgefeilteren Ansatz erfordern könnten, möglicherweise unter Verwendung von regulären Ausdrücken oder einer Parser-Bibliothek.

## Siehe auch

Für mehr Kontext zum Umgang mit Strings in Kotlin können Sie die offizielle Dokumentation einsehen:

- [Kotlin's String-Dokumentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)

Für tiefergehende Einblicke in reguläre Ausdrücke und das Parsen in Kotlin:

- [Kotlin Regex-Dokumentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
