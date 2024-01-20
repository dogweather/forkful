---
title:                "Å starte et nytt prosjekt"
html_title:           "C: Å starte et nytt prosjekt"
simple_title:         "Å starte et nytt prosjekt"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å starte et nytt prosjekt betyr å lage en ny kodebase fra scratch til å løse et spesifikt problem. Programmerere gjør det for å skape nye applikasjoner, implementere nye funksjoner eller lære nye programmeringsteknikker.

## Hvordan:
For å starte et nytt prosjekt i Kotlin bruker vi kommandolinjen. Her er et grunnleggende eksempel:

```Kotlin
$ mkdir MyProject && cd MyProject
$ touch Main.kt 
```
Etter å ha opprettet `Main.kt`, kan du starte kodingen din slik:

```Kotlin
fun main(args: Array<String>) {
    println("Hello, Kotlin!")
}
```

Når du kjører koden, vil du se følgende output:

```
Hello, Kotlin!
```
## Deep Dive
Å starte et nytt prosjekt i programmering har ikke alltid vært så lett som det er nå. Tidligere ble programmer skrevet i lavnivåspråk som C og C++, som krever mye mer kode for å oppnå det samme resultatet.

Når det gjelder alternativer, kan programmerere starte nye prosjekter i en rekke språk, ikke bare i Kotlin. Java, Python og JavaScript er også populære alternativer.

For å implementere detaljer i Kotlin begynner vi vanligvis med å definere klassene og funksjonene vi trenger, bygge hovedfunksjonen vår, og deretter legge til logikk for å svare på brukerens handlinger.

## Se også
Hvis du vil gå dypere, her er noen lenker til relaterte ressurser.

1. Kotlin offisielle dokumentasjon: https://kotlinlang.org/docs/home.html
2. 'Mastering Kotlin': En bok om mer avanserte Kotlin-teknikker.
3. 'Kotlin for Android Developers': En spesifikk guide for Android-utvikling med Kotlin.
4. GitHub repository of Kotlin projects: https://github.com/topics/kotlin