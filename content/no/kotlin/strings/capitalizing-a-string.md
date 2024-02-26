---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:44.183840-07:00
description: "\xC5 gj\xF8re om en streng til stor forbokstav i programmering inneb\xE6\
  rer \xE5 konvertere det f\xF8rste tegnet i strengen til en stor bokstav hvis det\
  \ ikke allerede er\u2026"
lastmod: '2024-02-25T18:49:38.912588-07:00'
model: gpt-4-0125-preview
summary: "\xC5 gj\xF8re om en streng til stor forbokstav i programmering inneb\xE6\
  rer \xE5 konvertere det f\xF8rste tegnet i strengen til en stor bokstav hvis det\
  \ ikke allerede er\u2026"
title: Sette stor bokstav i en streng
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å gjøre om en streng til stor forbokstav i programmering innebærer å konvertere det første tegnet i strengen til en stor bokstav hvis det ikke allerede er det, noe som er nyttig for formatering av brukerinndata eller visning av tekst i et brukergrensesnitt på en mer standardisert eller brukervennlig måte. Programmerere utfører denne operasjonen for å sikre datakonsistens eller for å møte spesifikke formateringskrav i sine programvareapplikasjoner.

## Hvordan:

I Kotlin kan strenger gjøres om til stor forbokstav ved hjelp av standardbibliotekfunksjoner uten behov for tredjepartsbiblioteker. Kotlins tilnærming til å håndtere strenger gjør disse operasjonene greie og konsise.

### Gjøre om hele strengen til stor forbokstav:

```kotlin
val melding = "hello, world!"
val storForbokstavMelding = melding.uppercase()

println(storForbokstavMelding) // Output: HELLO, WORLD!
```

### Gjøre om kun den første bokstaven til stor forbokstav:

Fra og med Kotlin 1.5 er `capitalize()`-funksjonen avskrevet og erstattet med en kombinasjon av `replaceFirstChar` og en lambda som sjekker om det er en liten bokstav for å transformere den til en stor bokstav.

```kotlin
val hilsen = "hello, world!"
val storForbokstavHilsen = hilsen.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(storForbokstavHilsen) // Output: Hello, world!
```

Denne tilnærmingen opprettholder resten av setningen i sin opprinnelige form mens kun den første bokstaven endres til en stor bokstav.
