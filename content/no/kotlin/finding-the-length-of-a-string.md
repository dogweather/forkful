---
title:                "Kotlin: Å finne lengden til en streng"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Strenger er en viktig del av enhver programmeringsoppgave, og en av de enkleste, men likevel nyttige operasjonene på en streng er å finne lengden på den. Å vite lengden på en streng kan hjelpe deg med å håndtere input, formatere utdata og generelt effektivisere koden din. I denne artikkelen vil vi se på hvordan du enkelt kan finne lengden på en streng ved hjelp av Kotlin.

## Hvordan

For å finne lengden på en streng i Kotlin, kan du bruke `length` metoden som er en del av `String` klassen. Her er et eksempel på hvordan du kan bruke denne metoden i koden din:

```Kotlin 
val navn = "Ole"
println(navn.length) 

// Output: 3
```

Som du kan se, er koden enkel og kort. Vi oppretter en variabel `navn` som inneholder strengen "Ole", og deretter bruker vi `length` metoden til å finne lengden på denne strengen. Denne metoden returnerer et heltall som representerer antall tegn i strengen.

Det er viktig å merke seg at `length` metoden teller hvert tegn, inkludert mellomrom og spesialtegn. Så hvis vi endrer strengen til "Ole Olsen", vil lengden være 9 i stedet for 7, fordi mellomrommet også blir talt med.

## Deep Dive

Nå som vi har sett på det grunnleggende for å finne lengden på en streng i Kotlin, la oss ta en dypere titt på hvordan denne metoden fungerer bak kulissene. Lengden på en streng blir bestemt ved å telle antall UTF-16-koder som kreves for å representere strengen. UTF-16-koder er numeriske verdier som representerer tegnene i en streng, og dette systemet brukes av Kotlin for å håndtere Unicode-tegn.

En annen viktig ting å merke seg er at `length` metoden er en egenskap av en streng og ikke en funksjon. Dette betyr at du ikke trenger å bruke parentesene `()` når du bruker denne metoden. For eksempel er disse to kodesnuttene like:

```Kotlin
val navn = "Ole"
println(navn.length)

val navn = "Ole"
println(navn.length())
```

## Se også

- [Kotlin Dokumentasjon om strenger](https://kotlinlang.org/docs/basic-types.html#strings)
- [Stack Overflow Diskusjon om å finne lengden på en streng i Kotlin](https://stackoverflow.com/questions/35243255/how-can-i-get-length-of-a-string-in-kotlin)