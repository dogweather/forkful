---
title:                "Kotlin: Lesing av en tekstfil."
simple_title:         "Lesing av en tekstfil."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Det er mange grunner til å lese en tekstfil i Kotlin. Kanskje du vil lagre eller behandle data fra en tekstfil, eller kanskje du vil lage et program som leser innholdet i en tekstfil og gjør noe med det. Uansett hva grunnen er, er det viktig å vite hvordan man leser en tekstfil riktig for å få ønsket resultat.

## Hvordan gjøre det
Den enkleste måten å lese en tekstfil i Kotlin er å bruke `readLines()` funksjonen. Den tar inn filens navn som en parameter, leser innholdet i filen og returnerer en liste med hver linje som et element. Her er et eksempel på hvordan du kan bruke `readLines()` funksjonen:

```Kotlin
val file = File("minTekstfil.txt")
val lines = file.readLines()

for (line in lines) {
    println(line)
}
```

Dette eksempelet vil lese innholdet i filen "minTekstfil.txt" og skrive ut hver linje til konsollen. Du kan også bruke `FileReader` og `BufferedReader` klassene til å lese en tekstfil. Her er et eksempel på hvordan du kan gjøre det:

```Kotlin
val file = File("minTekstfil.txt")
val fileReader = FileReader(file)
val bufferedReader = BufferedReader(fileReader)
var line = bufferedReader.readLine()

while (line != null) {
    println(line)
    line = bufferedReader.readLine()
}

bufferedReader.close()
```

Dette eksempelet gjør det samme som det første eksemplet, men det bruker en annen tilnærming. Det er viktig å merke seg at når du leser en tekstfil, må du sørge for å lukke filen etter at du er ferdig med å lese den. Dette gjøres ved å kalle `close()` metoden på `BufferedReader` objektet.

## Dypdykk
Når du leser en tekstfil, kan du også behandle og manipulere dataene på forskjellige måter. For eksempel kan du bruke `split()` funksjonen til å dele opp hver linje i mindre biter ved hjelp av et spesifikt tegn eller uttrykk. Dette kan være nyttig når du ønsker å lagre informasjonen i en database eller utføre beregninger på tall som er inkludert i tekstfilen.

En annen måte å lese en tekstfil på er å bruke `readText()` funksjonen, som returnerer innholdet i filen som en enkelt streng. Dette kan være nyttig når du for eksempel ønsker å søke etter et bestemt ord eller uttrykk i filen.

## Se også
- [Kotlin offisielle dokumentasjon](https://kotlinlang.org/docs/tutorials/kotlin-for-py/reading-files.html)
- [How to Read a Text File using Kotlin](https://dzone.com/articles/how-to-read-a-text-file-using-kotlin)
- [Working with Files in Kotlin](https://blog.kotlin-academy.com/working-with-files-in-kotlin-5e51bd4e1678?gi=9e3253a57a53)