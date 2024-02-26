---
date: 2024-01-20 17:40:54.004169-07:00
description: "Midlertidige filer er kortvarige datalagringselementer som skapes under\
  \ kj\xF8ring av programmer. Programmerere lager dem for \xE5 h\xE5ndtere store datastr\xF8\
  mmer,\u2026"
lastmod: '2024-02-25T18:49:38.948472-07:00'
model: gpt-4-1106-preview
summary: "Midlertidige filer er kortvarige datalagringselementer som skapes under\
  \ kj\xF8ring av programmer. Programmerere lager dem for \xE5 h\xE5ndtere store datastr\xF8\
  mmer,\u2026"
title: Opprette en midlertidig fil
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Midlertidige filer er kortvarige datalagringselementer som skapes under kjøring av programmer. Programmerere lager dem for å håndtere store datastrømmer, mellomlagre informasjon som ikke trenger å bli bevart langsiktig, eller når de tester funksjonaliteter uten å påvirke den faktiske datalagringen.

## Slik gjør du:
I Kotlin kan du enkelt opprette en midlertidig fil med `createTempFile`-funksjonen. Her er et eksempel:

```Kotlin
import java.io.File

fun main() {
    val tempFile: File = File.createTempFile("temp", ".tmp")

    println("Midlertidig fil opprettet: ${tempFile.absolutePath}")

    // Skriv noe til filen
    tempFile.writeText("Hei, dette er en test!")

    // Les fra filen
    val text = tempFile.readText()
    println("Filinnhold: $text")

    // Slett filen når den ikke lenger er nødvendig
    tempFile.deleteOnExit()
}
```

Eksempel på utskrift:

```
Midlertidig fil opprettet: /tmp/temp1234567890.tmp
Filinnhold: Hei, dette er en test!
```

## Dypdykk
Historisk har midlertidige filer vært en nødvendig del av programmering på grunn av begrensede minneressurser og behovet for å håndtere større datainnhold uten å belaste hovedlagring. I moderne systemer brukes de fortsatt for å redusere inn-/utskrivningsoperasjoner på permanente lagringsmedier, som kan være langsommere og mer slitende over tid. 

Alternativer til midlertidige filer inkluderer databaser eller in-memory databehandling med `ByteArrayOutputStream` eller liknende klasser i Kotlin, men disse teknikkene har egne trade-offs som høyere minneforbruk.

Implementering av midlertidige filer i Kotlin bygger på Java's I/O-API. `createTempFile` er en høy-nivå funksjon som automatisk generer et unikt filnavn og oppretter filen i systemets standardmappe for midlertidige filer, vanligvis `/tmp` på UNIX-lignende systemer. Funksjonens parametere tillater tilpassing av prefiks og suffiks i filnavnet. `deleteOnExit`-metoden er hendig for å slette filen når programmet avsluttes, men pass på å slette midlertidige filer manuelt om det er nødvendig.

## Se også
- [Kotlin Official Documentation](https://kotlinlang.org/docs/home.html)
- [Java File I/O (NIO.2)](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)
