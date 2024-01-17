---
title:                "Opprettelse av midlertidig fil"
html_title:           "Kotlin: Opprettelse av midlertidig fil"
simple_title:         "Opprettelse av midlertidig fil"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Opprettelse av et midlertidig fil er en måte å midlertidig lagre data på som skal slettes når det ikke lenger er nødvendig. Programmere kan gjøre dette for å spare plass eller hindre at filer blir liggende igjen etter utførelsen av programmet.

## Hvordan:
```Kotlin
val tempFile = createTempFile("navn", ".txt")
tempFile.writeText("Dette er en midlertidig tekstfil")
println(tempFile.readText())

// Utskrift:
//
// Dette er en midlertidig tekstfil
```

## Dypdykk:
Opprettelsen av midlertidige filer har vært en standard praksis i programmering i lang tid. Alternativer til å opprette et midlertidig fil inkluderer å bruke bufferminne eller å slette filen manuelt etter bruk. I Kotlin, brukes funksjonen `createTempFile ()` for å opprette en midlertidig fil.

## Se også:
[Dokumentasjon om opprettelse av midlertidige filer i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)