---
title:                "Lesing av tekstfil"
html_title:           "Kotlin: Lesing av tekstfil"
simple_title:         "Lesing av tekstfil"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lese en tekstfil i programmering betyr å åpne en fil og lese innholdet i den, enten som ren tekst eller strukturerte data. Dette er en vanlig oppgave for programmører, siden mange programmer trenger å hente data fra eksterne filer for å behandle informasjon eller manipulere filene selv.

## Hvordan:
Å lese en tekstfil i Kotlin er enkelt og kan gjøres ved hjelp av innebygde funksjoner og klasser. Først må filen importeres inn i prosjektet ditt ved hjelp av nøkkelordet "import". Deretter kan du bruke klassen "File" til å åpne filen og lese innholdet. Her er et eksempel på hvordan du kan lese en tekstfil i Kotlin:

```Kotlin
import java.io.File

fun main() {
    val fil = File("tekstfil.txt")
    //Åpne filen og lagre innholdet i en variabel
    val innhold = fil.readText()
    println(innhold)
}
```

Eksempeloutput: 
```
Dette er en tekstfil for illustrative formål. Denne filen vil bli lest av et Kotlin-program.
```

## Dykk dypere:
Å lese tekstfiler har vært en del av programmering siden begynnelsen, da det var en vanlig måte å lagre og behandle data på. I dag finnes det også andre måter å håndtere data, for eksempel med databasesystemer eller ved bruk av APIer. Det er også viktig å forstå forskjellen mellom å lese tekstfiler og binærfiler, som er formater som er mer egnet for å lagre og manipulere data som tall og bilder. Kotlin gir også muligheten til å skrive til tekstfiler ved hjelp av funksjoner som "writeText()" og "appendText()".

## Se også:
Her er noen ressurser for å lære mer om å lese og skrive tekstfiler i Kotlin:
- [Kotlin Dokumentasjon: Arbeide med filer](https://kotlinlang.org/docs/tutorials/kotlin-for-py/working-with-files.html)
- [Stack Overflow: Hvordan kan jeg lese en tekstfil i Kotlin?](https://stackoverflow.com/questions/22968818/how-to-read-a-text-file-in-kotlin)