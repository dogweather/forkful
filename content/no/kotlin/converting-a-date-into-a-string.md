---
title:                "Kotlin: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor
Datoer er et viktig konsept i programmering. Noen ganger vil du trenge å konvertere en dato til en tekststreng for å kunne vise den til brukeren eller lagre den i en database. I denne bloggposten vil jeg gå gjennom hvordan du kan konvertere en dato til en streng ved hjelp av Kotlin.

# Hvordan gjøre det
Det første du må gjøre er å importere `java.text.SimpleDateFormat` biblioteket. Dette vil gi deg alle verktøyene du trenger for å konvertere datoer til strenger. Deretter kan du bruke `SimpleDateFormat` klassen og dens metoder til å formatere og konvertere datoen.

```Kotlin
import java.text.SimpleDateFormat

val dato = Date() // Opprette et Date objekt med nåværende dato
val formatter = SimpleDateFormat("dd.MM.yyyy") // Angi ønsket format
val datoTekst = formatter.format(dato) // Konvertere dato til en streng

println(datoTekst) // Output: 05.09.2021
```

I eksempelet over oppretter vi først et `Date` objekt med nåværende dato. Deretter oppretter vi et `SimpleDateFormat` objekt og angir ønsket format for datostringen vår. Til slutt bruker vi `format()` metoden for å konvertere datoen til en streng.

# Dypdykk
Du kan også tilpasse formatet for datoen din ved å bruke forskjellige symboler i `SimpleDateFormat` klassen. For eksempel kan du legge til årstall med fire sifre ved å bruke `yyyy` i stedet for `yy`. Her er noen vanlige symboler som du kan bruke:

| Symbol | Beskrivelse           | Eksempel     |
| ------ | --------------------- | ------------ |
| d      | Dag i måneden         | 5            |
| dd     | Dag i måneden (to siffer) | 05           |
| M      | Måned i året          | 9            |
| MM     | Måned i året (to siffer) | 09           |
| yy     | Årstall (to siffer)   | 21           |
| yyyy   | Årstall (fire siffer) | 2021         |

For en full liste over symboler og deres bruk, anbefaler jeg å sjekke ut dokumentasjonen til `SimpleDateFormat` klassen.

# Se også
- [JavaDoc til SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Kotlin offisiell dokumentasjon](https://kotlinlang.org/docs/home.html)