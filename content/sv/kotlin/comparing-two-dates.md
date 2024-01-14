---
title:                "Kotlin: Jämföring av två datum"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Varför

Att jämföra två datum är en vanlig uppgift som många utvecklare stöter på i sitt arbete. Genom att kunna jämföra två datum kan du till exempel avgöra om ett datum ligger före eller efter ett annat, vilket kan vara användbart i diverse applikationer. I denna artikel kommer vi att ta en djupare titt på hur man kan jämföra datum i Kotlin.

# Så här gör du

För att jämföra två datum i Kotlin kan du använda metoden `compareTo()` på klassen `LocalDate`. Denna metod returnerar ett tal som indikerar om det första datumet är före, efter eller samma som det andra datumet. Här är ett exempel på hur det kan se ut:

```Kotlin
val date1 = LocalDate.of(2021, 10, 15)
val date2 = LocalDate.of(2021, 10, 20)

val result1 = date1.compareTo(date2)
val result2 = date2.compareTo(date1)

println(result1) // -1 eftersom date1 är före date2
println(result2) // 1 eftersom date2 är efter date1
```

Som du kan se returneras ett negativt tal om det första datumet är före det andra, ett positivt tal om det första datumet är efter det andra, och noll om datumen är samma.

Du kan också jämföra datum baserat på deras tidkomponenter genom att använda metoden `isBefore()` eller `isAfter()`. Dessa metoder tar ett `LocalDate` objekt och returnerar en boolean som indikerar om det första datumet är före eller efter det andra. Här är ett exempel:

```Kotlin
val date1 = LocalDate.of(2021, 10, 15)
val date2 = LocalDate.of(2021, 10, 20)

val result1 = date1.isBefore(date2)
val result2 = date2.isBefore(date1)

println(result1) // true eftersom date1 är före date2
println(result2) // false eftersom date2 är efter date1
```

# Djupdykning

När du jämför datumen i Kotlin är det viktigt att komma ihåg att datumen måste vara av typen `LocalDate`. Om du har ett `LocalDateTime` objekt kan du använda metoden `toLocalDate()` för att få ett `LocalDate` objekt som du sedan kan jämföra.

Det är också viktigt att notera att jämförelser av datumen tar hänsyn till tidszonen. Om du till exempel jämför två datum som ligger på olika sidor om midnatt i olika tidszoner, kan resultatet bli annorlunda än väntat. Detta är något att ha i åtanke när du arbetar med olika tidszoner.

# Se även

- [Kotlin Date and Time API](https://kotlinlang.org/docs/datetime.html)
- [Java LocalDate Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)