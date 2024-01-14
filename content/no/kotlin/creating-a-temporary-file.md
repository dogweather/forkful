---
title:                "Kotlin: Lage en midlertidig fil"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

I denne bloggposten skal vi se nærmere på hvordan man kan lage en midlertidig fil i Kotlin, og hvorfor det kan være nyttig i visse situasjoner. 

Noen ganger trenger man å opprette en midlertidig fil på datamaskinen sin for å lagre informasjon som bare trengs for en kort periode, for eksempel under prosessen med å skrive en større applikasjon. Dette kan være nyttig for å frigjøre systemressurser og holde orden på filhåndteringen i kodebasen. 

## Slik Gjør du

For å opprette en midlertidig fil i Kotlin kan du bruke File.createTempFile() metoden, som vil automatisk lage en unik filnavn og plassere filen i systemets midlertidige filmappe. Du kan også spesifisere et prefiks og en suffiks for filnavnet, og hvor filen skal plasseres, ved hjelp av metoden File.createTempFile(prefix, suffix, directory). 

Her er et eksempel på hvordan du kan opprette en midlertidig fil og skrive til den ved hjelp av FileOutputStream-objektet:

```
Kotlin
val tempFile = File.createTempFile("temp", ".txt")
val data = "Dette er en midlertidig fil"
val outputStream = FileOutputStream(tempFile)
outputStream.use {
    it.write(data.toByteArray())
}
```

Etter at koden over har blitt kjørt, vil en midlertidig fil med navnet "tempXXXX.txt" (hvor XXXX er en unik nummerkombinasjon) bli opprettet i systemets midlertidige filmappe. Innholdet "Dette er en midlertidig fil" vil også være skrevet til filen. 

## Dykk Dypere

Når man lager en midlertidig fil, vil den automatisk bli slettet når programmet avsluttes eller når filen blir stengt. Dette er for å hindre at unødvendige filer blir liggende igjen på datamaskinen. Men i noen tilfeller kan det være ønskelig å beholde filen eller slette den manuelt. Dette kan gjøres ved å sette filen til å ikke være en midlertidig fil ved hjelp av metoden File.deleteOnExit(). Filen vil da ikke bli slettet når programmet avsluttes, og man kan slette den manuelt når det trengs. 

## Se Også

- [Java File.createTempFile() dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Kotlin FileOutputStream dokumentasjon](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file-output-stream.html)