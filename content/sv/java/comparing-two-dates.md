---
title:                "Jämförelse av två datum"
html_title:           "Java: Jämförelse av två datum"
simple_title:         "Jämförelse av två datum"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad är det och varför?
Dagens programmeringsvärld är full av data och information som måste hanteras och jämföras. Ett vanligt problem är att jämföra två olika datum för att se om de är lika eller olika. Detta är ett grundläggande koncept inom programmering som hjälper till att organisera och manipulera datumuppgifter.

## Hur man gör:
För att jämföra två datum i Java, behöver du använda metoden "compareTo" från klassen "Date". Detta gör att du kan jämföra två datum och få en returvärde som indikerar deras förhållande till varandra.

```Java
Date date1 = new Date(21/01/2021);
Date date2 = new Date(20/01/2021);

int comparison = date1.compareTo(date2);

if(comparison == 0){
	System.out.println("Date1 är lika med Date2");
}
else if(comparison > 0){
	System.out.println("Date1 är senare än Date2");
}
else{
	System.out.println("Date1 är tidigare än Date2");
}
```
Output:
```
Date1 är senare än Date2
```

## Djupdykning:
Jämförelsen av datum i programmering har funnits sedan början av datorer. Detta koncept är grunden för många andra datumrelaterade funktioner och har utvecklats över tid. Det finns även alternativa metoder för att jämföra datum, såsom att konvertera dem till millisekunder och sedan jämföra dessa värden.

Det är viktigt att notera att "compareTo" inte är begränsad till endast datum. Det kan också användas för att jämföra andra typer av data, såsom stringar och numeriska värden, vilket gör det till ett allmänt användbart verktyg.

## Se också:
För mer information om date manipulation och andra användbara Java-metoder, kolla in följande länkar:

- [Java Date Class](https://www.w3schools.com/java/java_date.asp)
- [Oracle Documentation for Date Comparison](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html#compareTo-java.util.Date-)