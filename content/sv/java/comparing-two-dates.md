---
title:    "Java: Jämförande av två datum"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

Varför: Att jämföra två datum i Java kan vara användbart i många olika situationer, till exempel vid hantering av bokningar, registrering av tidsstämplar eller beräkning av ålder. I denna bloggpost kommer vi att gå igenom hur man jämför två datum i Java och vilka verktyg som finns tillgängliga för detta.

## Hur man jämför två datum i Java

För att jämföra två datum i Java finns det flera olika metoder och klasser som kan användas. Nedan följer några exempel:

```Java
// Skapa två datumobjekt för jämförelse
Date datum1 = new Date();
Date datum2 = new Date();

// Använd metoden compareTo() för att jämföra två datumobjekt
int resultat = datum1.compareTo(datum2);

// Om resultatet är 0 så är datumen lika, om det är mindre än 0 är datum1 tidigare än datum2, och om det är större än 0 är datum1 senare än datum2
if(resultat == 0){
  System.out.println("Datumen är lika.");
}
else{
  System.out.println("Datumen är inte lika.");
}
```

I detta exempel används metoden `compareTo()` som returnerar en integer som kan användas för att avgöra ordningen mellan två datum. Det finns flera andra metoder som också kan användas, till exempel `after()` och `before()`.

```Java
// Skapa två datumobjekt för jämförelse
LocalDate datum1 = LocalDate.now();
LocalDate datum2 = LocalDate.of(2020, 5, 14);

// Använd metoden isAfter() för att avgöra om ett datum är senare än ett annat
if(datum2.isAfter(datum1)){
  System.out.println("Datum 2 är senare än datum 1.");
}
```

I detta exempel används klassen `LocalDate` som finns tillgänglig från Java 8. Denna klass har metoder som `isAfter()` och `isBefore()` för att jämföra två datum i LocalDate-format.

## Djupdykning i jämförelse av datum

När man jämför datum i Java är det viktigt att vara medveten om att det finns olika typer av datumformat och klasser som kan användas. Det är också viktigt att hantera tidszoner och att konvertera mellan olika tidszoner om det behövs.

För att undvika felaktiga resultat bör man också vara noga med att jämföra datum på rätt nivå av precision. Till exempel bör man inte jämföra två datum baserat på timmar och minuter om de också innehåller sekunder.

I vissa fall kan det också vara nödvändigt att använda metoder för att konvertera ett datum till en annan typ, till exempel från `Date` till `LocalDate` eller vice versa.

## Se även

- [Java Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Java Date vs Calendar](https://www.baeldung.com/java-date-vs-calendar)
- [Joda-Time library](https://www.joda.org/joda-time/)