---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:00.955395-07:00
description: "Hur man g\xF6r: **Grundl\xE4ggande tolkning:** Metoderna `DateTime.Parse`\
  \ och `DateTime.TryParse` \xE4r de fr\xE4msta alternativen f\xF6r att omvandla en\
  \ str\xE4ng till ett\u2026"
lastmod: '2024-04-05T21:53:39.260254-06:00'
model: gpt-4-0125-preview
summary: "**Grundl\xE4ggande tolkning:** Metoderna `DateTime.Parse` och `DateTime.TryParse`\
  \ \xE4r de fr\xE4msta alternativen f\xF6r att omvandla en str\xE4ng till ett `DateTime`."
title: "Analysera ett datum fr\xE5n en str\xE4ng"
weight: 30
---

## Hur man gör:
**Grundläggande tolkning:**

Metoderna `DateTime.Parse` och `DateTime.TryParse` är de främsta alternativen för att omvandla en sträng till ett `DateTime`. Här är ett snabbt exempel:

```csharp
string dateString = "2023-04-12";
DateTime parsedDate;

if (DateTime.TryParse(dateString, out parsedDate))
{
    Console.WriteLine($"Lyckades tolka: {parsedDate}");
}
else
{
    Console.WriteLine("Misslyckades med att tolka.");
}
// Utdata: Lyckades tolka: 2023-04-12 00:00:00
```

**Specifiera en kultur:**

Ibland behöver du tolka en datumsträng som är i ett specifikt kulturformat. Detta kan du åstadkomma med hjälp av klassen `CultureInfo`:

```csharp
using System.Globalization;

string dateString = "12 avril 2023";
var cultureInfo = new CultureInfo("fr-FR");
DateTime parsedDate = DateTime.Parse(dateString, cultureInfo);

Console.WriteLine(parsedDate);
// Utdata: 2023-04-12 00:00:00
```

**Exakt tolkning med ett specifikt format:**

För scenarier där datum kommer i ett specifikt format som kanske inte är standard, är `DateTime.ParseExact` praktisk:

```csharp
string dateString = "Wednesday, 12 April 2023";
string format = "dddd, d MMMM yyyy";
DateTime parsedDate = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);

Console.WriteLine(parsedDate);
// Utdata: 2023-04-12 00:00:00
```

**Använda NodaTime:**

För ännu mer robust datum- och tidstolkning, överväg att använda det populära tredjepartsbiblioteket NodaTime. Det ger ett bredare utbud av funktioner för hantering av datum/tid:

```csharp
using NodaTime;
using NodaTime.Text;

var pattern = LocalDatePattern.CreateWithInvariantCulture("yyyy-MM-dd");
var parseResult = pattern.Parse("2023-04-12");

if (parseResult.Success)
{
    LocalDate localDate = parseResult.Value;
    Console.WriteLine(localDate); // 2023-04-12
}
else
{
    Console.WriteLine("Misslyckades med att tolka.");
}
```

NodaTime erbjuder omfattande stöd för tidszoner, begrepp för perioder och varaktigheter samt många olika kalendersystem, vilket gör det till ett kraftfullt val för komplex datum- och tidshantering i .NET-applikationer.
