---
title:    "Kotlin: Jämföra två datum."
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum kan vara en viktig del av att skriva effektiv och korrekt Kotlin-kod. Det kan hjälpa dig att kontrollera för utgångsdatum, validera ålder eller sortera information baserat på datum. I denna bloggpost kommer vi att utforska hur man jämför två datum i Kotlin och dess användbarhet i olika scenarier.

## Hur man gör

För att börja jämföra två datum i Kotlin behöver vi först importera klassen "LocalDate". Detta gör vi genom att lägga till följande kod i början av vårt Kotlin-dokument:

```Kotlin
import java.time.LocalDate
```

Nu kan vi tilldela två datumvärden till variabler och jämföra dem som följande:

```Kotlin
// Skapa två lokala datumvariabler
val datum1 = LocalDate.of(2021, 1, 1) // 1 januari 2021
val datum2 = LocalDate.of(2021, 3, 1) // 1 mars 2021

// Jämför datum1 och datum2
if (datum1 < datum2) {
    println("datum1 ligger före datum2")
} else if (datum1 > datum2) {
    println("datum1 ligger efter datum2")
} else {
    println("datum1 och datum2 är lika")
}

// Output: datum1 ligger före datum2
```

Som vi kan se jämför vi de två datumvariablerna genom att använda " < ", " > " eller " == " tecken beroende på vilket resultat vi vill uppnå. Vi kan också använda andra metoder från LocalDate-klassen som "isEqual" eller "isAfter" för att göra olika typer av jämförelser.

## Djupdykning

När vi jämför två datum i Kotlin finns det några saker vi bör tänka på. Först och främst är det viktigt att märka att LocalDate-klassen bara är giltig för dagar mellan åren 0001 och 9999. Detta innebär att om vi försöker jämföra datum utanför denna tidsram kan vi stöta på felaktigt beteende.

För det andra, när vi jämför datum i en loop eller i en fil kan det vara mer effektivt att använda metoden "compareTo" istället för att använda "<" eller ">" jämförelsetecken. Detta minskar antalet objekt som behöver skapas för innehåll, vilket kan vara en fördel i applikationer med hög prestanda.

Sist men inte minst är det också viktigt att notera att LocalDate-klassen är immutabel, vilket betyder att vi inte kan ändra ett datum efter att det har skapats. Det är också viktigt att vara medveten om olika tidszoner när vi jämför datum, eftersom det kan påverka resultatet.

## Se även

Läs gärna mer om hur man jämför datum i Kotlin genom att besöka följande länkar:

- https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date.html
- https://www.baeldung.com/kotlin-compare-dates
- https://www.geeksforgeeks.org/kotlin-localdate-compareto-function/

Tack för att du läste denna bloggpost om att jämföra datum i Kotlin. Hoppas det varit informativt och hjälpsamt för dig!