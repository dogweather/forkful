---
title:                "Jämförelse av två datum"
html_title:           "Kotlin: Jämförelse av två datum"
simple_title:         "Jämförelse av två datum"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Jämföra två datum är en vanligt förekommande uppgift inom programmering. Det handlar helt enkelt om att jämföra två datum för att se vilket som är tidigare eller senare. Detta är något som programmerare ofta behöver göra för att kunna hantera tidsbaserad information på ett effektivt sätt.

## Så här gör du:
För att jämföra två datum i Kotlin behöver du använda dig av klassen `LocalDate`. För att skapa ett datum använder du kommandot `LocalDate.of(year, month, day)`. Du kan sedan använda metoden `.isBefore()` eller `.isAfter()` för att jämföra två datum. Nedan följer ett exempel på hur detta kan se ut:

```Kotlin
val dateOne = LocalDate.of(2020, 5, 15)
val dateTwo = LocalDate.of(2020, 5, 20)

if (dateOne.isBefore(dateTwo)) {
    println("Datum 1 är tidigare än datum 2")
} else if (dateOne.isAfter(dateTwo)) {
    println("Datum 1 är senare än datum 2")
} else {
    println("Datumen är lika")
}
```

Det första datumet skapas med hjälp av kommandot `LocalDate.of(2020, 5, 15)` och det andra datumet skapas med kommandot `LocalDate.of(2020, 5, 20)`. Därefter använder vi `if`-satsen för att jämföra de två datumen med metoden `.isBefore()` och `.isAfter()`. Beroende på vilket datum som är tidigare eller senare skrivs ett lämpligt meddelande ut.

## Djupdykning:
Historiskt sett har det funnits många olika sätt att representera och räkna med datum och tid inom programmering. Ett vanligt problem var att olika länder använde olika datumformat, vilket gjorde det svårt att hantera tidsbaserad information på ett enhetligt sätt. Med tiden började språk som Java och Kotlin använda klassen `LocalDate` för att lösa detta problem.

En annan metod för att jämföra två datum är att använda `Calendar`-objektet och metoder som `before()` och `after()`. Detta är dock en äldre metod som inte är lika effektiv som att använda sig av klassen `LocalDate`.

När man jämför två datum i Kotlin jämförs datumet på sekundnivå. Det betyder att om två datum har samma år, månad och dag men skiljer sig på sekunderna så anses de fortfarande vara olika datum.

## Se även:
- [Kotlin Date and Time](https://kotlinlang.org/docs/reference/datetime.html)
- [Java LocalDate Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Comparison & operations on LocalDate objects in Kotlin](https://www.tutorialspoint.com/compare-and-operations-on-localdate-objects-in-kotlin)