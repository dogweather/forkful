---
title:                "Parsa ett datum från en sträng"
html_title:           "Kotlin: Parsa ett datum från en sträng"
simple_title:         "Parsa ett datum från en sträng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Parsing av ett datum från en sträng är när en programmerare konverterar en textsträng till ett datumformat som datorn kan förstå och hantera. Det är användbart för att sortera och filtrera data baserat på datum, samt för att visa datum i önskat format. 

## Hur man gör:
Kotlin har inbyggda funktioner för att enkelt parsa ett datum från en sträng. Nedan finns exempel på två olika sätt att parsea ett datum från en sträng.

```Kotlin
val str = "2021-03-22" // sträng med datum
val date = LocalDate.parse(str) // konverterar strängen till ett LocalDate-objekt
println(date.year) // output: 2021
```

Du kan också specificera det önskade datumformatet genom att använda DateTimeFormatter.

```Kotlin
val str = "03/22/2021" 
val formatter = DateTimeFormatter.ofPattern("MM/dd/yyyy") // specificerar datumformatet
val date = LocalDate.parse(str, formatter) // konverterar strängen till LocalDate-objekt
println(date.dayOfWeek) // output: MONDAY
```

## Djupdykning:
Parsing av datum från strängar har varit en utmaning för många programmerare, särskilt när olika datumformat används. Tidigare har man använt sig av reguljära uttryck eller tredjepartsbibliotek för att hantera det här problemet. Men med Kotlin's inbyggda funktioner är det nu enklare än någonsin att parsea datum från strängar.

## Se även:
- [Kotlin DateTimeFormatter](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-date-time-formatter/index.html)
- [Kotlin LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/index.html)