---
title:                "Kotlin: Hente nåværende dato"
simple_title:         "Hente nåværende dato"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er ofte nødvendig å få tilgang til dagens dato i et program. For eksempel kan man ønske å registrere når en hendelse skjedde, eller vise brukeren dagens dato i et grensesnitt. Heldigvis gjør Kotlin det enkelt å få tak i dagens dato, og i denne bloggposten skal vi se på hvordan vi kan gjøre det.

## Slik gjør du det

For å få dagens dato i Kotlin kan vi bruke klassen `LocalDate`, som tilhører Java Date and Time API. Vi kan opprette en ny instans av denne klassen ved å kalle statisk metode `now()` og deretter spesifisere ønsket tidssone. La oss ta en titt på et eksempel:

```Kotlin
val today = LocalDate.now(ZoneId.of("Europe/Oslo"))
println(today)
```

I dette eksempelet oppretter vi `today`-variabelen og tilordner den verdien av dagens dato i tidsstosonen 'Europe/Oslo'. Deretter bruker vi `println` for å vise datoen på skjermen. Outputen vil se noe liknende ut:

```
2021-04-23
```

Hvis vi ønsker å formatere datoen på en spesifikk måte, for eksempel som en streng, kan vi bruke `DateTimeFormatter` klassen. La oss se på et eksempel der vi formaterer datoen til å vise både dag og måned på norsk:

```Kotlin
val norskDatoFormat = DateTimeFormatter.ofPattern("d. MMMM", Locale("no"))
val formattetDato = today.format(norskDatoFormat)
println(formattetDato)
```

Output vil da bli:

```
23. april
```

## Dykk dypere

`LocalDate`-klassen kan også brukes til å manipulere datoen. For eksempel kan vi legge til eller trekke fra et antall dager, måneder eller år. La oss se på et eksempel:

```Kotlin
val futureDate = today.plusDays(10)
println(futureDate)
```

Vi oppretter en variabel `futureDate` og tilsetter 10 dager til dagens dato. Output vil bli datoen 10 dager fra nå:

```
2021-05-03
```

Det finnes også flere metoder for å sammenligne datoer, for eksempel `isAfter()`, `isBefore()` og `isEqual()`, som kan være nyttig i ulike situasjoner.

## Se også

- [Kotlin Dokumentasjon - Kotlin Date and Time](https://kotlinlang.org/docs/datetime.html)
- [Java Date and Time API Dokumentasjon](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)
- [ZoneId Java Dokumentasjon](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/ZoneId.html)