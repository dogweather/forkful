---
title:                "Konvertera ett datum till en sträng"
html_title:           "Kotlin: Konvertera ett datum till en sträng"
simple_title:         "Konvertera ett datum till en sträng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Ibland kan det vara användbart att konvertera ett datum till en sträng för att göra det läsbart för människor. Det kan också vara till hjälp när man arbetar med databaser eller API-anrop.

## Hur man gör
```Kotlin
// Skapa en instans av LocalDate som representerar ett datum
val date = LocalDate.of(2021, 9, 1)

// Konvertera datumet till en sträng med formatet "dd/MM/yyyy"
val dateString = date.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))

// Skriv ut den konverterade strängen
System.out.println(dateString)
// Output: 01/09/2021
```

Det finns flera olika format som du kan använda för att konvertera en datum till en sträng. Du kan också ange tidszon eller ändra separatorn för datumet.

## Djupdykning
När du konverterar ett datum till en sträng använder du dig av klassen `LocalDate` och metoden `format()` från klassen `DateTimeFormatter`. Det finns flera olika formatterare som du kan använda beroende på dina behov, till exempel `ofLocalizedDate()`, `ofPattern()` eller `ofLocalizedDateTime()`. Du kan också använda metoden `parse()` för att konvertera en sträng till ett datum.

Det kan finnas situationer där du behöver hantera datum på ett mer avancerat sätt, som när du arbetar med tidszoner eller behöver göra beräkningar baserat på datum. I sådana fall kan det vara användbart att använda sig av bibliotek som Joda-Time eller ThreeTen-Extra för att få mer omfattande funktioner för att hantera datum och tider.

## Se även
- [Kotlin LocalDate dokumentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/)
- [Java DateTimeFormatter dokumentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Joda-Time bibliotek](http://www.joda.org/joda-time/)
- [ThreeTen-Extra bibliotek](https://www.threeten.org/threeten-extra/)