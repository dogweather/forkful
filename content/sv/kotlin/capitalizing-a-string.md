---
title:                "Att göra en sträng versal"
date:                  2024-01-19
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kapitalisera en sträng betyder att omvandla bokstäverna till versaler (stora bokstäver). Programmerare gör detta för att standardisera textdata, till exempel för att visa titlar eller namn konsekvent.

## Så här gör du:
I Kotlin används metoden `.uppercase()` för att kapitalisera en sträng.

```Kotlin
fun main() {
    val exampleString = "kotlin är kul"
    val capitalizedString = exampleString.uppercase()

    println(capitalizedString) // Output: KOTLIN ÄR KUL
}
```

Metoden hanterar också svenska å, ä och ö så att de blir Å, Ä och Ö korrekt.

```Kotlin
fun main() {
    val swedishString = "vår älskade ö"
    val capitalizedSwedishString = swedishString.uppercase()

    println(capitalizedSwedishString) // Output: VÅR ÄLSKADE Ö
}
```

## Utförligare Information
Kapitalisering i programmering har funnits nästan lika länge som programmeringsspråken själva. I tidigare versioner av Kotlin användes `.toUpperCase()`, men den är nu ersatt med `.uppercase()` för att möta förändringar i standardbiblioteket och för att förbättra läsbarheten.

Det finns alternativ till `.uppercase()` när du vill manipulera strängar i Kotlin:

- `.lowercase()`: Gör alla bokstäver i en sträng till gemener (små bokstäver).
- `.capitalize()`: Ökar bara den första bokstaven i en sträng, men är föråldrad sedan Kotlin 1.5.
- `java.lang.String.toUpperCase(Locale)`: Om du behöver specificera en lokal inställning för kapitalisering, så som Turkiska som har speciella regler för tecknet 'i'.

Kapitalisering implementeras internationellt genom Unicode och tar därmed hänsyn till språkspecifika regler. I Kotlin är detta särskilt enkelt eftersom standardbiblioteksfunktionerna hanterar dessa komplexiteter åt dig.

## Se även
- Kotlin Standard Library documentation for `String`: [Kotlin String Documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- Unicode standard for case mapping: [Unicode Case Mapping](https://unicode.org/reports/tr21/)
- Kotlin style guide for idiomatic kotlin code: [Kotlin Coding Conventions](https://kotlinlang.org/docs/coding-conventions.html)
