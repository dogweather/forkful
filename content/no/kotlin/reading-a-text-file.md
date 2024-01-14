---
title:                "Kotlin: Lese en tekstfil"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne lese en tekstfil er en grunnleggende og viktig ferdighet for enhver programmerer. Det tillater deg å hente og behandle informasjon fra en fil på en enkel måte, og kan være nyttig i mange ulike programmeringsprosjekter.

## Hvordan

For å lese en tekstfil i Kotlin, må du følge noen enkle steg:

1. Definer en ```File```-objekt som representerer filen du ønsker å lese:
   
   ```Kotlin
   val fil = File("minfil.txt")
   ```

2. Åpne filen for lesing ved hjelp av ```BufferedReader```-objektet:
   
   ```Kotlin
   val bufferedReader = BufferedReader(FileReader(fil))
   ```

3. Les filen linje for linje ved hjelp av en ```while```-løkke:
   
   ```Kotlin
   var linje: String?
   while (bufferedReader.readLine().also { linje = it } != null) {
       // gjør noe med hver linje, for eksempel skrive den ut
       println(linje)
   }
   ```

4. Husk å lukke ```bufferedReader```-objektet når du er ferdig med å lese filen:
   
   ```Kotlin
   bufferedReader.close()
   ```

5. Kjør koden din og se resultatet i konsollen. Hver linje fra filen vil bli skrevet ut.

**Merk:** Det er også mulig å lese en tekstfil direkte inn i en streng ved hjelp av ```readText()```-metoden på ```File```-objektet.

## Dypdykk

Det er flere ting du bør være oppmerksom på når du leser en tekstfil i Kotlin. Her er noen ekstra tips og triks:

- Husk å håndtere eventuelle unntak som kan oppstå når du leser filen. Dette kan gjøres ved å legge til en ```try-catch```-blokk rundt kode som kan føre til et unntak.
- Du kan også lese filer på internett ved hjelp av ```URL```-objektet og ```openStream()```-metoden.
- Hvis filen du leser er stor, kan det være lurt å bruke ```readLines()```-metoden istedenfor å lese én linje om gangen. Denne metoden leser innholdet i filen som en liste av strenger, der hver streng representerer en linje i filen.
- Du kan også lese filer med andre typer innhold, som for eksempel CSV-filer (komma-separerte verdier) eller XML-filer, ved hjelp av spesialiserte biblioteker.

## Se også

- [Offisiell Kotlin-dokumentasjon for å lese filer](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-text.html)
- [Kotlin Programming av Lawrence Angrave](https://www.amazon.com/Kotlin-Programming-Beginner-Lawrence-Angrave/dp/1788994012)