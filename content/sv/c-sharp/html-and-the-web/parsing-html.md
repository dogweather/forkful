---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:42.257455-07:00
description: "Hur: Medan .NET tillhandah\xE5ller grundl\xE4ggande st\xF6d f\xF6r att\
  \ arbeta med HTML, som `HttpClient` f\xF6r att h\xE4mta webbsidor, saknar det en\
  \ inbyggd, omfattande\u2026"
lastmod: '2024-03-13T22:44:37.911056-06:00'
model: gpt-4-0125-preview
summary: "Medan .NET tillhandah\xE5ller grundl\xE4ggande st\xF6d f\xF6r att arbeta\
  \ med HTML, som `HttpClient` f\xF6r att h\xE4mta webbsidor, saknar det en inbyggd,\
  \ omfattande HTML-tolk."
title: Tolka HTML
weight: 43
---

## Hur:
Medan .NET tillhandahåller grundläggande stöd för att arbeta med HTML, som `HttpClient` för att hämta webbsidor, saknar det en inbyggd, omfattande HTML-tolk. Därför vänder sig de flesta C#-utvecklare till populära tredjepartsbibliotek som HtmlAgilityPack eller AngleSharp för robusta funktioner för HTML-tolkning. Båda biblioteken tillåter lätt frågeställning, manipulation och traversal av HTML DOM.

### Använda HtmlAgilityPack
1. **Installera HtmlAgilityPack**: Lägg först till HtmlAgilityPack-paketet i ditt projekt via NuGet.
   ```
   Install-Package HtmlAgilityPack
   ```

2. **Exempelkod**: Tolka en HTML-sträng och extrahera titlarna på alla `<h1>`-element.

   ```csharp
   using HtmlAgilityPack;
   using System;
   using System.Linq;

   class Program
   {
       static void Main(string[] args)
       {
           var html = @"<html>
                         <body>
                             <h1>Titel 1</h1>
                             <h1>Titel 2</h1>
                         </body>
                        </html>";
           var htmlDoc = new HtmlDocument();
           htmlDoc.LoadHtml(html);

           var h1Taggar = htmlDoc.DocumentNode.SelectNodes("//h1").Select(node => node.InnerText);
           foreach (var titel in h1Taggar)
           {
               Console.WriteLine(titel);
           }
       }
   }
   ```

   **Exempelutskrift:**
   ```
   Titel 1
   Titel 2
   ```

### Använda AngleSharp
1. **Installera AngleSharp**: Lägg till AngleSharp-biblioteket i ditt projekt via NuGet.
   ```
   Install-Package AngleSharp
   ```

2. **Exempelkod**: Ladda ett HTML-dokument och fråga efter `div`-element med en specifik klass.

   ```csharp
   using AngleSharp;
   using AngleSharp.Dom;
   using System;
   using System.Linq;
   using System.Threading.Tasks;

   class Program
   {
       static async Task Main(string[] args)
       {
           var context = BrowsingContext.New(Configuration.Default);
           var document = await context.OpenAsync(req => req.Content("<div class='item'>Artikel 1</div><div class='item'>Artikel 2</div>"));

           var artiklar = document.QuerySelectorAll(".item").Select(element => element.TextContent);
           foreach (var artikel in artiklar)
           {
               Console.WriteLine(artikel);
           }
       }
   }
   ```

   **Exempelutskrift:**
   ```
   Artikel 1
   Artikel 2
   ```

Både HTMLAgilityPack och AngleSharp är kraftfulla verktyg för att tolka HTML, men ditt val mellan dem kan bero på specifika projektbehov, prestandaöverväganden eller personliga preferenser i API-design.
