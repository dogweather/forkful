---
title:                "Omvandla ett datum till en sträng"
html_title:           "C#: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera ett datum till en sträng är en vanlig uppgift inom programmering, eftersom det gör det möjligt att visa datumet på ett läsbart sätt för användaren. Det är också användbart när man behöver skriva ut datumet i en text eller spara det i en fil.

## Hur man gör det
För att konvertera ett datum till en sträng i C# har vi tillgång till flera olika metoder som vi kan använda beroende på vårt specifika behov. Nedan följer några exempel med kod och utskrift för att hjälpa dig att förstå.

### Formatera datumet som en sträng
Ett vanligt sätt att konvertera datumet till en sträng är att använda den inbyggda metoden ToString(). Detta gör det möjligt att formatera datumet på olika sätt genom att använda olika formatsträngar. Här är ett exempel på hur man kan göra detta:

```C#
DateTime datum = DateTime.Now;
string datumSomSträng = datum.ToString("dd-MM-yyyy");
Console.WriteLine(datumSomSträng); 
```
Output: 05-08-2021

### Konvertera till en annan kultur
Om du behöver konvertera datumet till en sträng i en annan kultur, till exempel på svenska, kan du använda metoden ToString() tillsammans med klassen CultureInfo. Här är ett exempel på hur man kan göra detta:

```C#
DateTime datum = DateTime.Now;
string datumSomSträng = datum.ToString("dd MMMM yyyy", new CultureInfo("sv-SE"));
Console.WriteLine(datumSomSträng); 
```
Output: 05 augusti 2021

### Skapa en anpassad sträng från datumet
Förutom de inbyggda metoderna kan du också använda klassen StringBuilder för att skapa en anpassad sträng från datumet. Här är ett exempel på hur man kan göra detta:

```C#
DateTime datum = new DateTime(2021, 08, 05);
StringBuilder sträng = new StringBuilder();
sträng.Append("Idag är det ");
sträng.Append(datum.Month);
sträng.Append(" månad och ");
sträng.Append(datum.Year);
Console.WriteLine(sträng.ToString());
```
Output: Idag är det 8 månad och 2021

## Djupdykning
Det finns många olika formatsträngar som kan användas för att formatera datumet på olika sätt, inklusive dag, månad, år, timme, minut, sekund och mer. Du kan också använda metoder som ToString("d"), ToString("g") eller ToString("f") för att få en standardformatering baserat på den aktuella kulturen.

Det är viktigt att tänka på att varje plattform eller applikation kan ha sina egna regler för datumformat, så det kan vara bra att undersöka vad som är vanligt eller rekommenderas för det specifika projektet.

## Se även
- [C# DateTime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [C# CultureInfo](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-5.0)
- [String.Format Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.format?view=net-5.0)