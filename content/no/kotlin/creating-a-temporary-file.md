---
title:    "Kotlin: Opprettelse av en midlertidig fil"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Hvorfor

Det å lage midlertidige filer er nyttig når du ønsker å lagre data midlertidig, for eksempel når du jobber med store datasett eller når du vil teste koden din uten å permanent lagre endringene. Å opprette midlertidige filer er enkelt og kan gi deg bedre kontroll over dataene dine.

# Hvordan lage midlertidige filer i Kotlin

For å opprette en midlertidig fil i Kotlin, kan du bruke `createTempFile()` -funksjonen. Denne funksjonen tar to parametere: prefix og suffix. Prefixen er fornavnet på den midlertidige filen, mens suffixen er filtypen. Her er en enkel kode som viser hvordan du kan bruke denne funksjonen:

```Kotlin
val tempFile = createTempFile("bakery", ".txt")
```

For eksempel, hvis du ønsker å opprette en midlertidig fil med navnet "bakery" og filtypen ".txt", vil koden opprette en fil med navnet "bakery12345.txt" der "12345" er et tilfeldig tall generert av Kotlin. Dette sikrer at hver midlertidig fil som opprettes vil ha et unikt navn.

Du kan også spesifisere en sti der du ønsker å opprette den midlertidige filen. Dette gjøres ved å legge til en ekstra parameter i `createTempFile()` -funksjonen:

```Kotlin
val tempFile = createTempFile("bakery", ".txt", File("/Users/navn/Downloads"))
```

I dette tilfellet vil den midlertidige filen bli opprettet under stien "/Users/navn/Downloads".

Hvis du vil lagre dataene dine i den midlertidige filen, kan du bruke `printWriter()` -funksjonen. Dette vil tillate deg å skrive til filen som om det var en vanlig tekstfil:

```Kotlin
tempFile.printWriter().use { out ->
    out.println("Dette er en midlertidig fil som inneholder data fra en bakeri-app.")
}
```

Når du er ferdig med å bruke den midlertidige filen, kan du slette den ved å bruke `delete()` -funksjonen:

```Kotlin
tempFile.delete()
```

# Dykk dypere

Nå som du vet hvordan du oppretter midlertidige filer i Kotlin, er det nyttig å også vite hvor disse filene blir lagret. Som standard opprettes de midlertidige filene i operativsystemets midlertidige mappe. Hvis du vil ha full kontroll over hvor filene blir lagret, kan du bruke `setTemporaryDirectory()` -funksjonen til å endre standardplasseringen.

Du kan også bruke `ListFiles()` -funksjonen for å få tilgang til alle de midlertidige filene som er opprettet i løpet av kjøretiden til programmet ditt.

# Se også

- [Kotlin offisiell dokumentasjon](https://kotlinlang.org/docs/reference/)
- [Enkel guide til å bruke filhåndtering i Kotlin](https://www.baeldung.com/kotlin-file-handling)