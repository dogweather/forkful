---
title:                "Kotlin: Att få den aktuella datumen"
simple_title:         "Att få den aktuella datumen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hämta den nuvarande datumet är en viktig funktion i många Kotlin-program. Det kan användas för att visa tidsstämplar, planera och schemalägga uppgifter, eller bara för att hålla spår på vilken dag det är. I denna blogginlägg kommer vi att gå igenom hur man enkelt kan hämta det nuvarande datumet med hjälp av Kotlin.

## Hur man gör

För att få det nuvarande datumet i Kotlin kan du använda klassen `LocalDate`. Detta ger dig datumet i dagens lokala tidszon. För att hämta datumet behöver du bara skriva följande kod:

```kotlin
val date = LocalDate.now()
println(date)
```

Kör koden och du kommer att se det nuvarande datumet i konsolen.

Om du vill ha mer detaljerad information om datumet, som till exempel vilken veckodag det är eller hur många dagar det är i det aktuella året, kan du använda klassen `LocalDate.now()` tillsammans med olika metoder. Här är ett exempel som visar datumet i ett specifikt format:

```kotlin
val date = LocalDate.now()
val dayOfWeek = date.dayOfWeek // hämtar veckodagen
val dayOfMonth = date.dayOfMonth // hämtar dagen i månaden
val month = date.month // hämtar månaden
val year = date.year // hämtar året
println("Idag är det ${dayOfWeek}, den ${dayOfMonth} ${month} ${year}") // skriver ut datumet i ett format som "Torsdag, den 1 juli 2021"
```

Om du vill få datumet i en annan tidszon kan du använda metoden `atZone()` och ange den önskade tidszonen som ett argument. Här är ett exempel:

```kotlin
val date = LocalDate.now().atZone(ZoneId.of("America/New_York"))
println(date)
```

Du kan också använda `LocalDate.now()` tillsammans med klassen `DateTimeFormatter` för att få datumet i önskat format. Det finns många fördefinierade formateringsalternativ, men du kan också skapa dina egna. Här är ett exempel där vi använder en anpassad formatterare för att få datumet i formatet "dd/MM/yyyy":

```kotlin
val date = LocalDate.now()
val formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy") // använder en anpassad formatterare
val formattedDate = formatter.format(date) // formaterar datumet enligt den anpassade formatteraren
println(formattedDate) // skriver ut datumet i formatet "01/07/2021"
```

## Djupdykning

Om du vill ha ännu mer kontroll över datumet, kan du använda klassen `LocalDate` tillsammans med klassen `LocalDateTime`. LocalDateTime innehåller även tid och kan användas för att skapa specifika datum och tider. Här är ett exempel där vi skapar en LocalDateTime och sedan hämtar datumet:

```kotlin
val dateTime = LocalDateTime.of(2021, Month.JULY, 1, 12, 0, 0) // skapar en lokal datetime med år, månad, dag, timme, minut och sekund
val date = dateTime.toLocalDate() // hämtar datumet
println(date) // skriver ut datumet "2021-07-01"
```

Det finns också många andra användbara metoder i klassen `LocalDate`, som till exempel `plusDays()` och `minusDays()` för att lägga till eller dra av dagar från datumet. Du kan utforska alla tillgängliga metoder på Kotlin's officiella dokumentationssida för `LocalDate`.

### Se även

- [Kotlin's officiella dokumentation för LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/)
- [Kotlin's officiella dokumentation för LocalDateTime](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date-time/)
- [DateTimeFormatter-klassens dokumentation](https://kotlinlang.org/api/latest/jvm/stdlib/java.time/-date-time-formatter/)