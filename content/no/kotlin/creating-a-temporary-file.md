---
title:                "Kotlin: Lage en midlertidig fil"
simple_title:         "Lage en midlertidig fil"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Temporary files er en viktig del av moderne programmering, spesielt når det gjelder å håndtere midlertidig data eller å sikre programkjøringen. Ved å skrive Kotlin-kode for å lage midlertidige filer, kan du enkelt håndtere disse situasjonene og forbedre funksjonaliteten til ditt program.

## Hvordan
Det finnes flere måter å lage en midlertidig fil i Kotlin på. La oss ta en titt på to av de mest vanlige metodene.

Først må du importere klassen som er nødvendig for å lage en midlertidig fil:

```Kotlin
import java.io.File
```

### Metode 1: createTempFile()
Den enkleste måten å lage en midlertidig fil i Kotlin er ved å bruke funksjonen `createTempFile()`. Denne funksjonen tar inn tre parametere: et prefix for filnavnet, et suffix for filnavnet og en filbane der den midlertidige filen skal lagres. Prefix og suffix er valgfrie, og hvis ingen blir gitt, vil en standardverdi bli brukt.

```Kotlin
val temporaryFile = File.createTempFile("temp_", ".txt", "/path/to/directory")
```

I eksempelet ovenfor blir en midlertidig fil opprettet med prefikset "temp_" og suffixet ".txt". Filen vil bli lagret i mappen "/path/to/directory" og vil ha et unikt nummer som er generert automatisk.

### Metode 2: writeFileToTemp()
Den andre metoden er å bruke funksjonen `writeFileToTemp()`. Denne funksjonen tar inn én parameter: et filnavn. Funksjonen vil opprette en midlertidig fil med det angitte navnet og returnere en `File`-objekt som kan brukes til å skrive data til filen.

```Kotlin
val temporaryFile = File("my_temp_file.txt")
val fileToWrite = temporaryFile.writeFileToTemp("my_temp_file.txt")
```

I dette eksempelet blir en tom midlertidig fil med navnet "my_temp_file.txt" opprettet. Deretter blir en `File`-objekt returnert, som vi kan bruke til å skrive data til den midlertidige filen.

## Deep Dive
Når en midlertidig fil blir opprettet, vil den bli lagret i systemets temp-mappe. Denne mappen er vanligvis skjult og er forskjellig fra system til system. Det kan derfor være lurt å hente banen til denne mappen ved hjelp av `System.getProperty("java.io.tmpdir")`. På denne måten vil du ha en universell måte å få tak i banen til temp-mappen på, uavhengig av operativsystem.

En annen ting å merke seg er at midlertidige filer ikke blir slettet automatisk. Det er derfor viktig å sørge for å slette dem når de ikke lenger er nødvendige. Dette kan gjøres ved å kalle `delete()`-funksjonen på `File`-objektet.

## Se også
- [Official Kotlin documentation on temporary files](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [Java.io.File documentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Creating temporary files in Java](https://www.baeldung.com/java-temporary-file)