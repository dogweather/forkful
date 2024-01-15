---
title:                "Les en tekstfil"
html_title:           "Kotlin: Les en tekstfil"
simple_title:         "Les en tekstfil"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese en tekstfil er en vanlig handling for programmerere, uansett hvilket språk de bruker. I denne artikkelen vil vi utforske hvordan dette gjøres i Kotlin og hvorfor det er viktig.

## Hvordan

```Kotlin
import java.io.File

fun main(args: Array<String>) {
    val file = File("tekstfil.txt")

    //Lese filinnholdet linje for linje
    file.forEachLine { println(it) }

    //Lese alt innholdet som en enkeltstreng
    val fileContent = file.readText()
    println(fileContent)
}

```

### Eksempelutgang:

Linje 1
Linje 2
Linje 3

Linje 1
Linje 2
Linje 3

## Deep Dive

Å lese en tekstfil innebærer å få tilgang til informasjon som er lagret på en strukturert måte. Det kan være alt fra å lese data fra en database til å hente informasjon fra en API. I Kotlin brukes File-klassen fra java.io-pakken til å åpne og lese en fil. Deretter kan man bruke enkle metoder som forEachLine() for å lese og behandle dataen linjeforlinje.

Det er også viktig å være klar over at når man leser en tekstfil, leser man dataen i sin opprinnelige form. Dette kan bety at man må konvertere informasjonen til riktig format før man kan behandle den videre.

## Se også 
- [Kotlin Docs - File Class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Reading and Writing Files in Kotlin](https://www.baeldung.com/kotlin-file-handling)