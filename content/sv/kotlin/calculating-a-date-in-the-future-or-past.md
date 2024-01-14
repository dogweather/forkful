---
title:                "Kotlin: Beräkna ett datum i framtiden eller förflutna"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna datum i framtiden eller det förflutna är en viktig förmåga inom programmering. Det kan hjälpa dig att hantera tidsbaserade uppgifter och göra dina program mer dynamiska och användbara.

## Hur man gör

För att kunna räkna ut ett datum i framtiden eller det förflutna i Kotlin, behöver du först och främst använda funktionen `Calendar`. Detta är ett inbyggt klassobjekt i Kotlin som innehåller många användbara metoder för hantering av datum.

Låt oss säga att vi vill beräkna vilken dag det kommer att vara om 45 dagar från idag. Vi kan göra det genom att skapa en instans av `Calendar` och använda metoden `add` för att lägga till 45 dagar till det nuvarande datumen.

```Kotlin
val kalender = Calendar.getInstance()
kalender.add(Calendar.DAY_OF_YEAR, 45)
println(kalender.time)
```

Detta kommer att skriva ut datumet som är 45 dagar från idag, i det format som är inställt av ditt operativsystem.

Om du vill beräkna ett datum i det förflutna, använd bara negativa värden i `add`-metoden. Till exempel, om du vill veta vilken dag det var för 2 veckor sedan, kan du göra det genom att använda `-14` som argument.

## Djupdykning

För att förstå mer om hur datumberäkningar fungerar i Kotlin, måste vi förstå vissa begrepp. `Calendar`-objektet använder sig av en tidslinje som kallas "Era", "År" och "Dag". Genom att ändra dessa värden kan vi åstadkomma olika beräkningar.

När vi använder `add`-metoden, använder vi alltid värden baserade på denna tidslinje. Till exempel, om vi vill lägga till ett år till det nuvarande datumet, skulle vi använda `Calendar.YEAR` som argument i `add`-metoden.

Det finns också andra metoder som `set` och `roll` i `Calendar` som kan användas för att justera datumet på olika sätt. Det är också möjligt att ställa in specifika datum och tider genom att ange värden för "År", "Månad" och "Dag" i `set`-metoden.

## Se även

- [Kotlin's Calendar Class Documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-calendar/index.html)
- [Beräkna veckodag i Java/ Kotlin](https://www.baeldung.com/java-day-of-week)
- [Sun's Official Guide to the Java Calendar](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)