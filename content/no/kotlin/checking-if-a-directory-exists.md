---
title:                "Kontrollere om en mappe eksisterer"
html_title:           "Kotlin: Kontrollere om en mappe eksisterer"
simple_title:         "Kontrollere om en mappe eksisterer"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kontrollere om en mappe eksisterer er en viktig del av programmering fordi det tillater deg å håndtere og håndtere eventuelle feil som kan oppstå når du jobber med filer og mapper. Dette bidrar til å sikre at koden din fungerer jevnt og forhindrer potensielle krasjer eller feil i programmene dine.

## Hvordan å

Kotlin tilbyr en enkel og effektiv måte å sjekke om en mappe eksisterer ved å bruke ```kotlin File() ``` -konstruktøren og ```kotlin exists()``` metoden. Her er et eksempel på hvordan du kan gjøre det:

```
val directoryPath = File("C:/Users/User/Documents")
if (directoryPath.exists()) {
    println("Mappen eksisterer!")
} else {
    println("Mappen eksisterer ikke.")
}
```

I dette eksempelet definerer vi en variabel som representerer mappen vi ønsker å sjekke. Deretter bruker vi ```exists()``` metoden til å sjekke om mappen faktisk eksisterer eller ikke. Hvis mappen eksisterer, skriver vi ut en melding som sier det, ellers skriver vi ut en annen melding.

## Dykk dypere

Når du sjekker om en mappe eksisterer, kan det noen ganger være nyttig å få mer informasjon om mappen, for eksempel dens størrelse eller når den ble opprettet. Dette kan gjøres ved å bruke forskjellige metoder tilgjengelig i Kotlin, for eksempel ```getName()```, ```length()``` og ```lastModified()```.

En annen ting å vurdere er hva som skjer dersom du prøver å kjøre kode som jobber med en mappe som ikke eksisterer. I disse tilfellene vil koden din kaste en ```FileNotFoundException```, så det er viktig å håndtere dette på en riktig måte i koden din for å sikre at den ikke krasjer.

## Se også

- Dokumentasjon for Codereview: [Kontrollere om en mappe eksisterer i Kotlin](https://www.codereview.com/check-if-directory-exists-kotlin)
- Dokumentasjon for Kotlin: [Kotlin Filklasse](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)