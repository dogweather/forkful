---
title:                "Oppretting av en midlertidig fil"
html_title:           "Kotlin: Oppretting av en midlertidig fil"
simple_title:         "Oppretting av en midlertidig fil"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til at man trenger å opprette en midlertidig fil under programmering. Det kan være for å lagre midlertidige data, teste kodesnutter eller håndtere filbehandling på en sikker måte.

## Slik gjør du det

For å opprette en midlertidig fil i Kotlin trenger vi å bruke standard biblioteket `java.io.File` og `createTempFile()` metoden. Denne metoden tar imot to argumenter: prefiks og sufiks til filnavnet. Prefikset kan være hva som helst, for eksempel "temp" eller "test", mens sufikset må være en gyldig filtype, for eksempel ".txt" eller ".tmp". 

```Kotlin
val tempFile = File.createTempFile("temp", ".txt")
```

Nå har vi en midlertidig fil som heter "temp1234.txt" liggende i systemets katalog for midlertidige filer. Vi kan også spesifisere en bestemt katalog hvor vi ønsker å opprette filen ved å legge til et tredje argument som er en `File`-instans.

```Kotlin
val tempFile = File.createTempFile("temp", ".txt", File("C:/Temp"))
```

For å skrive data til den midlertidige filen kan vi bruke `FileWriter` og `BufferedWriter` som følger:

```Kotlin
val writer = BufferedWriter(FileWriter(tempFile))
writer.write("Dette er en midlertidig fil.")
writer.close()
```

Når vi er ferdig med å bruke den midlertidige filen, kan vi slette den ved å kalle `delete()` metoden på `File`-instansen.

```Kotlin
tempFile.delete()
```

## Dykke dypere ned

Når vi oppretter en midlertidig fil, blir den automatisk slettet når programmet avsluttes. Vi kan imidlertid også spesifisere en levetid for filen ved å legge til ett eller flere av argumentene `deleteOnExit()` og `delete()` på `File`-instansen. Dette kan være nyttig når vi trenger å beholde den midlertidige filen i litt lenger tid, for eksempel når vi bruker den i en test.

```Kotlin
tempFile.deleteOnExit()
```

Vi kan også spesifisere en annen katalog for de midlertidige filene ved å sette en systemvariabel kalt `java.io.tmpdir`. Dette kan være nyttig hvis vi ikke ønsker å fylle opp systemets standard katalog for midlertidige filer.

```Kotlin
System.setProperty("java.io.tmpdir", "C:/Temp")
```

## Se også

- [Kotlins offisielle dokumentasjon om java.io.File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Ekspertråd om midlertidige filer i programmering fra Stack Overflow](https://stackoverflow.com/questions/19951406/when-to-use-temporary-files-in-programming)