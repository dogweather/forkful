---
title:                "Sjekke om en katalog eksisterer"
html_title:           "Kotlin: Sjekke om en katalog eksisterer"
simple_title:         "Sjekke om en katalog eksisterer"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?

Å sjekke om en mappe eksisterer er en måte å verifisere om en bestemt mappe finnes på datamaskinen. Dette er nyttig for programmerere fordi det lar dem utføre spesifikke handlinger avhengig av om mappen eksisterer eller ikke.

## Hvordan:

```Kotlin
fun main() {

    val dir = File("example/directory/")
    if (dir.exists()) {
        println("Mappen finnes!")
    } else {
        println("Mappen finnes ikke.")
    }
}
```

Eksempelutgang:
```
Mappen finnes ikke.
```

## Dypdykk:

1. Historisk kontekst: Sjekking av mapper begynte å bli mer vanlig på 1980-tallet med utviklingen av grafiske brukergrensesnitt, og har siden blitt et vanlig prosedyre i mange programmeringsspråk.
2. Alternativer: I stedet for å bruke innebygde funksjoner, kan utvidelser eller tredjepartsbiblioteker brukes til å få tilgang til mapper og utføre handlinger.
3. Implementeringsdetaljer: I Kotlin er det vanlig å bruke File-klassen for å sjekke om en mappe eksisterer. Dette gjøres ved å opprette et File-objekt med banen til mappen og bruke funksjonen exists() for å sjekke om den finnes.

## Se også:

- [File API documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Kotlin for Android development](https://developer.android.com/kotlin/)
- [Alternative libraries for file operations in Kotlin](https://kotlinresources.com/libraries/file-operations/)