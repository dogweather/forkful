---
title:                "Kotlin: Sjekke om en mappe eksisterer"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor bør man sjekke om en mappe eksisterer i Kotlin-kode? Vel, det kan være flere årsaker til dette. For det første kan det hjelpe deg med å kontrollere at du ikke prøver å åpne eller opprette en mappe som allerede finnes, som kan føre til feil i koden din. Det kan også gi deg muligheten til å ta ulike handlinger avhengig av om mappen eksisterer eller ikke, for eksempel å opprette den hvis den ikke finnes, eller å gi brukeren en feilmelding hvis den ikke eksisterer.

## Slik gjør du det
Det er flere måter å sjekke om en mappe eksisterer i Kotlin-kode. Her er to forskjellige metoder som du kan bruke:

```Kotlin
// Metode 1: Bruke innebygde funksjoner
val mappenavn = "minMappe"
val finnesMappen = File(mappenavn).exists() // returnerer en Boolean verdi

if(finnesMappen) {
    println("Mappen $mappenavn finnes allerede.")
} else {
    println("Mappen $mappenavn eksisterer ikke.")
}

```

```Kotlin
// Metode 2: Bruke Paths-klassen
val mappenavn = "minMappe"
val mappeSti = Paths.get(mappenavn)
val finnesMappen = Files.exists(mappeSti) // returnerer en Boolean verdi

if(finnesMappen) {
    println("Mappen $mappenavn finnes allerede.")
} else {
    println("Mappen $mappenavn eksisterer ikke.")
}

```

Som du kan se i eksemplene over, bruker både metode 1 og 2 innebygde funksjoner for å kontrollere om mappen eksisterer. Metode 2 legger også til et ekstra trinn ved å bruke Paths-klassen for å få tak i mappenavnet i form av en sti. Begge metodene vil returnere en Boolean verdi som enten er true eller false, avhengig av om mappen eksisterer eller ikke.

## Dykk dypere
Hvis du ønsker å dykke dypere inn i konseptet med å sjekke om en mappe eksisterer, kan du også utforske andre metoder og funksjoner som kan bidra til å gjøre koden din mer robust og nøyaktig. Noen eksempler på dette kan være å sjekke om mappen er skrivbar, om den inneholder visse filer eller undermapper, og mer.

## Se også
- [Kotlin Docs: Checking file and directory existence](https://kotlinlang.org/docs/working-with-files.html#checking-file-and-directory-existence)
- [Kotlin Docs: Paths](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.nio.-paths/index.html)
- [Java Docs: java.io.File](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)