---
title:                "Att tolka ett datum från en sträng."
html_title:           "C#: Att tolka ett datum från en sträng."
simple_title:         "Att tolka ett datum från en sträng."
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När man pratar om "att parsra ett datum från en sträng" syftar man på att konvertera en sträng som innehåller ett datum (t.ex. "1/1/2020") till en datum-variabel som kan användas i ens program. Programmerare gör detta för att kunna hantera och manipulera datum i sitt kodande.

## Så här gör du:
Det finns flera sätt att parsra datum från en sträng i C#. Ett exempel är att använda DateTime.TryParse-metoden, där du anger strängen och den variabel som datumet ska sparas i:

```C#
string dateStr = "1/1/2020";
DateTime date;
var success = DateTime.TryParse(dateStr, out date);
Console.WriteLine(date); // Output: 1/1/2020 12:00:00 AM
```

Du kan också använda DateTime.ParseExact-metoden om du vill specificera ett visst datumformat:

```C#
string dateStr = "Jan 1, 2020";
DateTime date = DateTime.ParseExact(dateStr, "MMM d, yyyy", null);
Console.WriteLine(date); // Output: 1/1/2020 12:00:00 AM
```

En annan möjlighet är att använda DateTime.Parse-metoden, som automatiskt försöker tolka det angivna datumet i olika format.

## Djupdykning:
Parsing av datum från strängar är en vanlig uppgift inom programmering, och C# erbjuder flera inbyggda metoder för att göra detta. Det finns också externa bibliotek som erbjuder ytterligare funktioner för att hantera datum och tid. När man använder DateTime.TryParse-metoden är det viktigt att notera att den returnerar en bool-variabel för att ange om parsningen var lyckad eller inte.

## Se även:
- [DateTime.TryParse-metoden (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tryparse?view=netframework-4.8)
- [DateTime.ParseExact-metoden (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact?view=netframework-4.8)
- [DateTime.Parse-metoden (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse?view=netframework-4.8)