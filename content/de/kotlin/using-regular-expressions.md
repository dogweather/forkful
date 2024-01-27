---
title:                "Einsatz von regulären Ausdrücken"
date:                  2024-01-19
html_title:           "Bash: Einsatz von regulären Ausdrücken"
simple_title:         "Einsatz von regulären Ausdrücken"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke sind Muster, die Text nach bestimmten Regeln durchsuchen. Programmierer nutzen sie, um Text zu überprüfen, zu zerlegen und zu manipulieren, weil sie mächtig und effizient sind.

## Anleitung:
```kotlin
fun main() {
    val text = "Die Email-Adresse ist kontakt@example.com."
    val regex = "\\S+@\\S+".toRegex()

    val found = regex.find(text)
    println(found?.value)  // Ausgabe: kontakt@example.com
}
```

```kotlin
fun extractPhoneNumbers(text: String): List<String> {
    val phoneRegex = "\\+\\d{2,3}(\\s|-)\\d{3,4}(\\s|-)\\d{6,7}".toRegex()
    return phoneRegex.findAll(text).map { it.value }.toList()
}

fun main() {
    val text = "Peter's Telefon ist +49 1234 567890, Hanna's ist +48 9876 543210."
    val phoneNumbers = extractPhoneNumbers(text)
    phoneNumbers.forEach { println(it) }  // Ausgabe: +49 1234 567890
                                           //          +48 9876 543210
}
```

## Tiefgang:
Reguläre Ausdrücke, oft abgekürzt als Regex, stammen aus der theoretischen Informatik und wurden erstmals in den 1950er Jahren beschrieben. Alternativen zu Regex sind spezifische Parser, die jedoch oft mehr Code erfordern und schwerer zu warten sind. Die Nutzung von RegEx in Kotlin erfolgt über die Java-Regex-API, da Kotlin auf der JVM läuft.

## Siehe Auch:
- [Kotlin Dokumentation zu regulären Ausdrücken](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [RegExr: Online-Tool zum Testen von regulären Ausdrücken](https://regexr.com/)
- [Java Pattern Klasse](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html) (da Kotlin die Java API verwendet)
