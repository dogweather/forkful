---
title:                "Konvertere en dato til en streng"
html_title:           "Kotlin: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor ville noen ønske å konvertere en dato til en streng? Dette kan være nyttig når man trenger å vise en dato på en spesifikk måte, for eksempel i et grensesnitt eller en rapport.

## Hvordan gjøre det

Kotlin har en innebygd funksjon for å konvertere en dato til en streng. La oss se på et enkelt eksempel:

```Kotlin
val date = LocalDate.now()
val dateString = date.toString()
println(dateString) //Utdata: 2021-09-03
```

Her oppretter vi en ny variabel `date` som inneholder dagens dato. Deretter bruker vi `toString()` -funksjonen for å konvertere datoen til en streng, som lagres i variabelen `dateString`. Til slutt skriver vi ut denne strengen, som vil vise formatet "ÅÅÅÅ-MM-DD".

Du kan også spesifisere et annet format for datostrengen ved å bruke `DateTimeFormatter` -klassen. La oss si at vi ønsker å vise datoen som DD.MM.ÅÅÅÅ:

```Kotlin
val formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
val dateString = date.format(formatter)
println(dateString) //Utdata: 03.09.2021
```

Her oppretter vi en formatteringsvariabel med det ønskede formatet, og deretter bruker vi `format()` -funksjonen med denne variabelen for å konvertere datoen til ønsket format.

## Dypdykk

Når man konverterer en dato til en streng, er det viktig å merke seg at formatet vil avhenge av lokal innstilling. Det vil si at hvis du kjører koden på en datamaskin med en annen lokal innstilling, kan det hende at datoen vises i et annet format.

Det finnes også andre alternativer for å konvertere en dato til en streng, for eksempel ved å bruke biblioteker som Joda-Time. Disse alternativene gir mer fleksibilitet og kontroll over datoformatet, men også legger til ekstra avhengigheter i prosjektet.

## Se også

- [Kotlin Strings](https://kotlinlang.org/docs/strings.html)
- [Java 8 DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)