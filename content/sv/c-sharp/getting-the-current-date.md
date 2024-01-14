---
title:    "C#: Att få den aktuella datumen"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Varför

Att både förstå och kunna hantera datum är en grundläggande del av programmering, oavsett vilket språk man använder sig av. Att kunna skapa, manipulera och hantera datum är viktigt för många olika applikationer och projekt. I den här bloggposten ska vi titta på hur man enkelt kan få fram dagens datum i C#.

## Såhär gör du

För att få fram dagens datum i C# behöver man bara några få rader kod. Vi kommer att använda ett inbyggt objekt i .NET frameworket, DateTime, för att få fram dagens datum. Här är ett exempel på hur koden kan se ut:

```C#
// Skapa ett DateTime objekt som representerar dagens datum
DateTime idag = DateTime.Now;

// Skriv ut dagens datum i konsolen
Console.WriteLine("Idag är det: " + idag.ToString("d"));

// Skriv ut dagens veckodag i konsolen
Console.WriteLine("Veckodag: " + idag.DayOfWeek.ToString());
```

Om vi kör den här koden så får vi följande utskrift i konsolen:

```
Idag är det: 2021-06-23
Veckodag: Wednesday
```

Som du kan se så är resultatet formaterat enligt det svenska datumformatet, "ÅÅÅÅ-MM-DD", och veckodagen är också på svenska.

Det finns också möjlighet att använda andra format för att skriva ut datumet. Till exempel kan du använda "D" för att skriva ut dagens datum på ett annat sätt. Här är ett exempel på hur koden kan se ut med det formatet:

```C#
// Skriv ut dagens datum i konsolen med annat format
Console.WriteLine("Idag är det: " + idag.ToString("D"));
```

Om vi kör koden igen så får vi följande utskrift:

```
Idag är det: onsdag den 23 juni 2021
```

Det finns också möjlighet att få ut mer detaljerad information om dagens datum, som till exempel vilken vecka på året det är eller om det är en skottår. För att få fram den här informationen så kan man använda sig av olika metoder och egenskaper på DateTime objektet. Det finns också möjlighet att göra beräkningar på datum, som att räkna ut antal dagar mellan två datum. De flesta av dessa funktioner finns i .NET frameworket och är lättillgängliga.

## Djupdykning

Att arbeta med datum i C# kan vara både enkelt och avancerat. Det finns många inbyggda funktioner och metoder som kan underlätta arbetet med datum. Det är också viktigt att förstå skillnaden mellan olika tidzoner och hur man kan hantera dessa i C#.

Det finns också möjlighet att använda externa bibliotek och paket för att göra arbetet med datum ännu smidigare. Det finns till exempel bibliotek som låter dig enkelt beräkna olika tidsintervall, som månader eller år, mellan två datum.

Att ha en god förståelse för hur man kan arbeta med datum i C# är viktigt för att kunna bygga stabila och pålitliga applikationer. Det är också en del av den grundläggande kunskapen inom programmering och något som är värt att ha koll på oavsett vilken typ av projekt du arbetar med.

## Se även

- [DateTime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [Standard och anpassade datum- och tidformat i .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings)
- [NodaTime – ett populärt paket för att hantera datum och tider i .NET](https://nodatime.org/)