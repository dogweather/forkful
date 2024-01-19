---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

---

## Vad & Varför?
Att hämta aktuellt datum innebär att erhålla datumet för nuvarande dag direkt från systemklockan. Det är användbart för programmerare att ha denna funktion för att kunna spåra händelser, generera tidsstämpel, schemalägga uppgifter och mycket mer.

## Hur gör man:
Att få tag på dagens datum i C# är en ganska enkel process. Nedan är en kodsnutt som visar hur man gör detta:

```C#
DateTime nu = DateTime.Now;
Console.WriteLine($"Dagens datum är: {nu.ToShortDateString()}");
```

När du kör den här koden skulle den se ut så här:

```
Dagens datum är: 28/12/2021
```
Notera att datumet kommer att variera beroende på den nuvarande datumet.

## Djupdykning
Historiskt sätt, C# har alltid tillhandahållit inbyggda metoder för tid och datum i `DateTime` klassen. För att få aktuellt datum använder vi `DateTime.Now` vilket ger oss en `DateTime`-instans, vilket innebär att vi kan komma åt dag, månad, år, timme, minut och sekund för det nuvarande datumet och tiden.

Även om `DateTime.Now` är det mest använda alternativet för att få nuvarande datum och tid, erbjuder C# även `DateTime.UtcNow` och `DateTime.Today`. `DateTime.UtcNow` ger den aktuella datumen och tiden i UTC-format medan `DateTime.Today` ger dagens datum vid midnatt (00:00:00).

Vad gäller internt implementation, när `DateTime.Now` anropas, hämtas systemklockan för datorn och konverteras till en läsbar form som programmerare kan använda och manipulera i deras kod.

## Se Även
Här är några usefulla länkar relaterade till att arbeta med datum och tider i C#:

- Microsoft dokumentation om DateTime: https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0
- Tutorial på arbeta med datum och tider i C#: https://www.c-sharpcorner.com/article/date-and-time-data-types-in-c-sharp/
- C# Date and time formatter: https://www.csharp-examples.net/string-format-datetime/