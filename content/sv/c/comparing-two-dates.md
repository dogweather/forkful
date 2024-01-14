---
title:                "C: Jämföring av två datum"
simple_title:         "Jämföring av två datum"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

##Varför

Att jämföra två datum är en vanlig uppgift som många programmerare stöter på i sitt arbete. Det kan vara användbart när man ska sortera datum eller beräkna skillnaden mellan två tidsperioder. I denna bloggpost kommer jag att gå igenom hur man kan jämföra två datum i C-programmering, så att du kan använda dig av det i dina egna projekt.

##Hur man gör

För att jämföra två datum i C, behöver vi använda oss av standard biblioteket time.h. Detta bibliotek ger oss tillgång till olika funktioner och datatyper som hjälper oss att hantera datum och tider.

För att kunna jämföra två datum behöver vi först spara dem i variabler. Vi kan använda datatypen "struct tm" för att lagra ett datum, som består av år, månad, dag osv. Exempelvis:

```C
struct tm date1 = {2020, 05, 20}; //Första datumet
struct tm date2 = {2019, 11, 03}; //Andra datumet
```

För att sedan jämföra dessa datum kan vi använda oss av funktionen "difftime()" som finns i time.h biblioteket. Denna funktion räknar ut skillnaden i sekunder mellan två datum. Om resultatet är negativt, innebär det att det första datumet inträffade före det andra datumet. Om det är positivt så inträffade det första datumet senare än det andra.

För att få resultatet i antal dagar istället för sekunder, kan vi dela resultatet med konstanten "86400", vilket motsvarar antalet sekunder på en dag. Exempelvis:

```C
double diff = difftime(mktime(&date1), mktime(&date2)); //Skillnaden i sekunder
int days = (int)(diff/86400); //Skillnaden i antal dagar
printf("Skillnaden mellan datum 1 och datum 2 är %d dagar", days); 
```

##Djupdykning

Det är viktigt att notera att funktionen "difftime()" inte tar hänsyn till tidszonen, utan bara räknar ut skillnaden i tid mellan två datum. Om du behöver ta hänsyn till tidszonen, måste du använda dig av andra funktioner och konvertera tid till UTC-format.

Det är också viktigt att se till att "struct tm" variablerna är korrekt formaterade, annars kan resultatet bli felaktigt. Datumet måste följa formatet år-månad-dag, och tid måste följa formatet timme:minut:sekund.

Med denna information kan du enkelt jämföra två datum i dina egna C-program.

##Se även

För mer information om datum och tider i C-programmering, rekommenderar vi följande länkar:

- [time.h Referens](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Tutorial om Datum och Tid i C](https://www.programiz.com/c-programming/c-date-time).
- [UTC Konverterare](https://www.savetime.zone/timezoneconverter/) för att konvertera mellan olika tidszoner.