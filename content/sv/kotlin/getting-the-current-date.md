---
title:                "Att få nuvarande datum"
html_title:           "Kotlin: Att få nuvarande datum"
simple_title:         "Att få nuvarande datum"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Att få den nuvarande datumet i en Kotlin-applikation är en enkel men viktig uppgift för programmerare. Det hjälper till att hålla spår på tidsrelaterad data och göra viktiga beslut baserade på aktuell tid. Det är också användbart för att hantera tidszoner och datumformatering i en applikation.

# Hur?

Använd nedanstående kodexempel för att få den aktuella datumet i Kotlin:

```Kotlin
val currentDate = java.util.Date()
println(currentDate)
```
Output:
```
Sun May 02 14:22:37 CEST 2021
```

Du kan också få datumet i ett specifikt format genom att använda `SimpleDateFormat` klassen och `format()` metoden. Till exempel:

```Kotlin
val sdf = SimpleDateFormat("dd/MM/yyyy")
val currentDate = sdf.format(java.util.Date())
println(currentDate)
```
Output:
```
02/05/2021
```

# Djupdykning

Förutom att använda standard Java-bibliotek kan du också använda Kotlin Date and Time API för att hantera datum och tid. Detta API tillhandahåller en mängd olika metoder och klasser som gör det enkelt att manipulera datum och tid i en applikation.

Alternativt kan du använda bibliotek som Joda-Time och ThreeTenABP för mer avancerade funktioner och bättre prestanda inom hantering av datum och tid.

Vid implementering, se alltid till att hantera datum och tid noga eftersom det kan påverka användarnas upplevelse och applikationens funktion. Se till att hantera tidszoner korrekt och undvika buggar och felaktigheter relaterade till datum och tid.

# Se även

- Kotlin Date and Time API dokumentation: https://kotlinlang.org/docs/datetime.html
- Joda-Time bibliotek: https://www.joda.org/joda-time/
- ThreeTenABP bibliotek: https://github.com/JakeWharton/ThreeTenABP