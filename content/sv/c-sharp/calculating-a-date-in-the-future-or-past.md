---
title:                "Beräkning av datum i framtiden eller det förflutna"
html_title:           "C#: Beräkning av datum i framtiden eller det förflutna"
simple_title:         "Beräkning av datum i framtiden eller det förflutna"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Det kan finnas många anledningar till varför man skulle vilja beräkna ett datum i framtiden eller förflutna. Det kan vara för att planera ett evenemang, planera resor eller kontrollera deadlines för projekt. Oavsett vad anledningen är, kan det vara väldigt användbart att kunna göra detta med C#.

## Så här gör du

För att börja, måste du först definiera startdatumet och antalet dagar som ska läggas till eller dras bort. Detta kan göras på flera olika sätt, men det vanligaste är att använda DateTime-strukturen i C#. Detta ger dig möjlighet att enkelt hantera datum och tider i din kod.

```C#
// Definiera ett startdatum
DateTime startDatum = new DateTime(2021, 04, 20);

// Lägg till eller dra bort antalet dagar
DateTime framtidaDatum = startDatum.AddDays(14);
DateTime förflutnaDatum = startDatum.AddDays(-7);

// Skriv ut resultaten
Console.WriteLine("Datumet 14 dagar framåt är: " + framtidaDatum.ToShortDateString());
Console.WriteLine("Datumet 7 dagar bakåt är: " + förflutnaDatum.ToShortDateString());

// Output:
// Datumet 14 dagar framåt är: 5/4/2021
// Datumet 7 dagar bakåt är: 4/13/2021
```

Detta är en enkel kod som lägger till eller drar bort dagar från ett visst datum och skriver ut resultatet. Du kan också göra mer avancerade beräkningar genom att använda andra metoder och egenskaper från DateTime-strukturen, som till exempel års-, månads- och veckonummer.

## Djupdykning

Det finns många olika sätt att hantera beräkningar av datum i C#, och det beror på vad du vill uppnå. Om du till exempel behöver hantera tidszoner eller sommartider, finns det speciella klasser och metoder som kan hjälpa dig med detta. Du kan också använda regex för att bearbeta datumsträngar eller använda TimeSpan för att beräkna skillnaden mellan två datum och tider.

Det är också viktigt att ha i åtanke att olika kulturer kan ha olika format för datum och tider. Därför kan du behöva använda CultureInfo-klassen för att hantera detta och säkerställa att dina beräkningar är korrekta för den specifika kulturen.

Sammanfattningsvis, för att kunna hantera datumberäkningar i C#, måste du ha en grundläggande förståelse för DateTime-strukturen och dess metoder och egenskaper. Men det är också användbart att ha kännedom om andra hjälpklasser och metoder som kan hjälpa dig att hantera olika scenarier.

## Se även

- [DateTime Struktur (C# referens)](https://docs.microsoft.com/sv-se/dotnet/api/system.datetime?view=net-5.0)
- [CultureInfo Klass (C# referens)](https://docs.microsoft.com/sv-se/dotnet/api/system.globalization.cultureinfo?view=net-5.0)
- [TimeSpan Struktur (C# referens)](https://docs.microsoft.com/sv-se/dotnet/api/system.timespan?view=net-5.0)
- [Regex Klass (C# referens)](https://docs.microsoft.com/sv-se/dotnet/api/system.text.regularexpressions.regex?view=net-5.0)