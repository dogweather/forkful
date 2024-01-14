---
title:    "C#: Jämföra två datum"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Varför

När du arbetar med datum i ett C# program, kan det ibland vara nödvändigt att jämföra två olika datum. Det kan vara för att kontrollera om ett datum ligger före eller efter ett annat, eller om båda datumen är samma. I denna bloggpost kommer vi att titta på hur man enkelt kan jämföra två datum i C#.

## Hur man gör

Enklaste sättet att jämföra två datum i C# är att använda sig av DateTime.CompareTo() metoden. Den här metoden jämför två Datetime-objekt och returnerar en hel siffra som indikerar om det första datumet är större, mindre eller lika med det andra datumet.

```C#
DateTime datum1 = new DateTime(2021, 04, 16);
DateTime datum2 = new DateTime(2021, 04, 20);

int resultat = datum1.CompareTo(datum2);

if (resultat < 0)
{
    Console.WriteLine($"{datum1} ligger före {datum2}");
}
else if (resultat > 0)
{
    Console.WriteLine($"{datum1} ligger efter {datum2}");
}
else
{
    Console.WriteLine($"{datum1} och {datum2} är samma datum");
}

// Output: 2021-04-16 ligger före 2021-04-20
```

I det här exemplet skapar vi två Datetime-objekt och använder sedan CompareTo-metoden för att jämföra dem. Beroende på resultatet, skrivs sedan ett lämpligt meddelande ut.

En annan metod för att jämföra datum är att använda DateTime.CompareToExact() metoden. Den här metoden fungerar på samma sätt som CompareTo, men ger dig även möjlighet att specificera en tidszon eller ett kalendersystem. Detta är användbart om du behöver jämföra datum som är i olika tidszoner eller kalendersystem.

```C#
DateTime datum1 = new DateTime(2021, 04, 16, 13, 05, 00);
DateTime datum2 = new DateTime(2021, 04, 16, 12, 05, 00);

int resultat = datum1.CompareToExact(datum2, DateTimeKind.Local);

if (resultat < 0)
{
    Console.WriteLine($"{datum1} ligger före {datum2}");
}
else if (resultat > 0)
{
    Console.WriteLine($"{datum1} ligger efter {datum2}");
}
else
{
    Console.WriteLine($"{datum1} och {datum2} är samma datum");
}

// Output: 2021-04-16 13:05:00 ligger före 2021-04-16 12:05:00
```

## Djupdykning

När man jämför datum i C#, är det viktigt att förstå hur DateTime-objektet fungerar. Ett Datetime-objekt består av datum, tid och en tidszon. När man jämför datumen måste man därför ta hänsyn till alla tre delar för att få ett korrekt resultat.

Det är även viktigt att vara försiktig med tidszoner och sommartid, som kan påverka hur datumet visas och jämförs. Använd därför alltid DateTime.ToUniversalTime() metoden för att konvertera ett datum till UTC (koordinerad universell tid) innan du jämför det.

## Se även

- [DateTime.CompareTo() Metod](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compareto?view=net-5.0#System_DateTime_CompareTo_System_DateTime_)
- [DateTime.CompareToExact() Metod](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.comparetoexact?view=net-5.0#System_DateTime_CompareToExact_System_DateTime_System_DateTimeKind_)