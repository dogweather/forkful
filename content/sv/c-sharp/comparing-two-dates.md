---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Att jämföra två datum innebär att du bedömer vilket datum som är tidigare, senare eller om båda datumen är identiska. Programmers gör detta för att sortera händelser, beräkna tidsperioder eller att hantera tidsstyrda operationer.

## Hur man gör:
Här kommer ett par exempel om hur du jämför två datum med C#.

```C#
DateTime startDate = new DateTime(2021, 3, 1);
DateTime endDate = new DateTime(2022, 3, 1);

int result = DateTime.Compare(startDate, endDate);

if (result < 0)
   Console.WriteLine("startDate är mindre än endDate.");
else if (result == 0)
   Console.WriteLine("Båda datumen är identiska.");
else
   Console.WriteLine("startDate är större än endDate.");
```

Sample Output:
```C#
startDate är mindre än endDate.
```

## Djupdykning:
Historiskt sett, datumjämförelser har spelat en vital roll i tidbaserad programmering och datahantering. I tidiga programmeringsspråk, var datumjämförelser inte så raka på sak. Men med moderna språk som C#, datumhantering och jämförelse har blivit mycket mer lättare och intuitiv. 

När det gäller alternativ, du kan också använda metoder som 'Equals()', 'CompareTo()' etc. för att jämföra datum i C#. 

Angående implementation, 'DateTime.Compare()' tittar på Ticks egenskapen för båda DateTime objekten för att avgöra vilket är tidigare eller senare. En 'Tick' representerar hundradelar av en sekund.

## Se även:
- DateTime.Compare Method på Microsoft Docs: [https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare?view=net-5.0](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare?view=net-5.0)
- C# DateTime tutorial på C# Station: [http://csharp-station.com/Tutorial/CSharp/Lesson17](http://csharp-station.com/Tutorial/CSharp/Lesson17)
- C# DateTime i detalj på TutorialsTeacher: [https://www.tutorialsteacher.com/csharp/csharp-datetime](https://www.tutorialsteacher.com/csharp/csharp-datetime)