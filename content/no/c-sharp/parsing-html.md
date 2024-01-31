---
title:                "Analyse av HTML"
date:                  2024-01-20T15:30:28.953683-07:00
html_title:           "Arduino: Analyse av HTML"
simple_title:         "Analyse av HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av HTML er prosessen med å lese og forstå HTML-koden slik at vi kan hente ut eller manipulere dataene. Programmerere gjør dette for å automatisere interaksjon med websider, trekke ut informasjon, eller teste webgrensesnitt.

## Slik gjør du:
C# har ikke innebygd støtte for HTML-parsing. Vi bruker ofte et bibliotek som HtmlAgilityPack. Her er en rask guide for å komme i gang:

```C#
using System;
using HtmlAgilityPack;

class Program
{
    static void Main()
    {
        var url = "https://example.com";
        var web = new HtmlWeb();
        var doc = web.Load(url);

        foreach (var node in doc.DocumentNode.SelectNodes("//a[@href]"))
        {
            Console.WriteLine($"Link: {node.Attributes["href"].Value}");
        }
    }
}
```
Eksempelresultat:
```
Link: https://example.com/about
Link: https://example.com/contact
```

## Dypdykk
HTML-parsing startet tidlig i webens dager for å gjøre innhold tilgjengelig for ulike systemer. HtmlAgilityPack, lansert i 2005, er en robust løsning for .NET utviklere. Alternativer inkluderer AngleSharp som tilbyr moderne asynkron API og full CSS3-støtte. Ved parsing er det viktig å huske på at HTML fra eksterne kilder kan variere i struktur og kvalitet - det er viktig å skrive robust kode som kan håndtere dette.

## Se Også
- HtmlAgilityPack på NuGet: https://www.nuget.org/packages/HtmlAgilityPack/
- AngleSharp GitHub-side: https://github.com/AngleSharp/AngleSharp
- C# Dokumentasjon: https://docs.microsoft.com/en-us/dotnet/csharp/
