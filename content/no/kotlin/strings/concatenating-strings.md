---
date: 2024-01-20 17:35:38.950381-07:00
description: "I programmering, \xE5 sl\xE5 sammen strenger (concatenating strings)\
  \ er rett og slett \xE5 sette dem sammen i \xE9n. Vi gj\xF8r det for \xE5 bygge\
  \ setninger, lage meldinger\u2026"
lastmod: '2024-03-13T22:44:40.742086-06:00'
model: gpt-4-1106-preview
summary: "I programmering, \xE5 sl\xE5 sammen strenger (concatenating strings) er\
  \ rett og slett \xE5 sette dem sammen i \xE9n."
title: "Sammensl\xE5ing av strenger"
weight: 3
---

## Hvordan:
I Kotlin er det flere måter å slå sammen strenger på. Her er et par eksempler:

```kotlin
fun main() {
    // Eksempel 1: Pluss-operatoren
    val hilsen = "Hei, " + "verden!"
    println(hilsen) // Skriver ut: Hei, verden!

    // Eksempel 2: String templates med "$"
    val navn = "Ola"
    val hilsning = "Hallo, $navn!"
    println(hilsning) // Skriver ut: Hallo, Ola!

    // Eksempel 3: .plus()-funksjonen
    val start = "Kotlin "
    val slutt = "rocks"
    val helSats = start.plus(slutt)
    println(helSats) // Skriver ut: Kotlin rocks

    // Eksempel 4: StringBuilder for lengre og mer kompliserte strenger
    val builder = StringBuilder()
    builder.append("Kotlin ")
    builder.append("er ")
    builder.append("fantastisk!")
    println(builder.toString()) // Skriver ut: Kotlin er fantastisk!
}
```

## Dykket Ned:
Historisk sett, i de eldste programmeringsspråkene, var strengsammenslåing ikke alltid så rett frem som den er i moderne språk. Noen språk krevde spesielle funksjoner eller til og med loops for å oppnå dette.

I Kotlin, samt mange moderne språk, er strengsammenslåing både enkel og intuitiv. Det er viktig å tenke på ytelse når du jobber med ekstremt store strenger eller i operasjoner som krever rask utførelse. I slike tilfeller kan `StringBuilder` være mer effektiv enn `+`, fordi den reduserer antall midlertidige objekter som opprettes under sammenslåingen.

Alternativer til de tradisjonelle metodene inkluderer bruk av string templates (som i Eksempel 2), hvor variabler og uttrykk enkelt kan plasseres direkte inn i en streng uten bruk av pluss-operatoren. Dette gjør koden mer lesbar og kortere.

Implementeringsdetaljer: Kotlin kompilerer strengsammenslåing med `+` til en `StringBuilder` under panseret når det kompilerer til Java bytecode, så ytelsen er ofte like god. Dog kan eksplisitt bruk av `StringBuilder`, som i Eksempel 4, gi deg mer kontroll og ytelse ved svært store eller komplekse strengsammenslåinger.

## Se Også:
- Kotlin dokumentasjon for strenger: [Kotlin String Documentation](https://kotlinlang.org/docs/basic-types.html#string-literals)
- En artikkel om ytelse ved strengsammenslåing: [String Concatenation Performance](https://proandroiddev.com/kotlin-pearls-string-concatenation-59ad72ee8b48)
- `StringBuilder` klassen i Kotlin: [Kotlin StringBuilder](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)
