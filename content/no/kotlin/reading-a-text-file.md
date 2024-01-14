---
title:    "Kotlin: Lesing av en tekstfil"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Hvorfor

Å lese en tekstfil er en vanlig oppgave for utviklere, spesielt i Kotlin. Det kan være nyttig å lære hvordan man gjør dette for å kunne håndtere større datamengder eller håndtere filer til og fra et nettverk.

# Hvordan

Det første vi må gjøre er å opprette en `File`-instans som representerer den tekstfilen vi ønsker å lese. Deretter må vi bruke en `BufferedReader` for å lese filen linje for linje. Dette gjøres ved å bruke `readLine()`-metoden til `BufferedReader`, som returnerer en `String`. Vi kan for eksempel printe ut hver linje til konsollen ved å bruke følgende kode:

```Kotlin
val fil = File("tekstfil.txt")
BufferedReader(FileReader(fil)).use { reader ->
    var linje = reader.readLine()
    while (linje != null) {
        println(linje)
        linje = reader.readLine()
    }
}
```

Dette vil printe hver linje i tekstfilen til konsollen. Det er også mulig å lese hele filen som en `String` ved å bruke `readText()`-metoden til `File`-instansen.

Det er viktig å merke seg at vi må bruke `try-catch` for å håndtere potensielle unntak som kan oppstå under lesingen av filen. For eksempel kan filen være utilgjengelig eller så kan det oppstå problemer med nettverket.

# Dykk dypere

Det finnes flere måter å lese tekstfiler på i Kotlin, som for eksempel å bruke `Scanner` eller `BufferedInputStream`. Det er også viktig å være klar over forskjellen mellom å lese filen som en sekvens av `Byte`-er eller som en `Char`-sekvens.

Det kan også være nyttig å se på hvordan man kan utføre forskjellige operasjoner på filen mens man leser den, som for eksempel å søke etter bestemte ord eller linjer.

# Se også

- [Standard bibliotek for File i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Lesing og skriving av filer i Kotlin](https://kotlinlang.org/docs/tutorials/kotlin-for-py/read-write-files.html)