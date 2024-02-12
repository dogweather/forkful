---
title:                "HTML parsen"
aliases:
- /de/c-sharp/parsing-html.md
date:                  2024-02-03T19:12:26.825658-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML parsen"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen von HTML in der Programmierung beinhaltet die Analyse der Struktur eines HTML-Dokuments, was Ihnen erlaubt, dessen Inhalt programmatisch zu extrahieren, zu manipulieren und zu interagieren. Programmierer machen dies, um das automatisierte Web-Scraping, die Datenextraktion oder sogar die dynamische Modifikation von Webseiten oder HTML-Dokumenten für verschiedene Anwendungen zu automatisieren, was es zu einer essenziellen Fähigkeit in der Webentwicklung, Datenanalyse und in automatisierten Testszenarien macht.

## Wie:

Während .NET grundlegende Unterstützung für die Arbeit mit HTML bietet, wie den `HttpClient` zum Abrufen von Webseiten, fehlt es an einem integrierten, umfassenden HTML-Parser. Daher wenden sich die meisten C#-Entwickler an beliebte Drittanbieter-Bibliotheken wie HtmlAgilityPack oder AngleSharp für robuste HTML-Parsing-Fähigkeiten. Beide Bibliotheken ermöglichen einfaches Abfragen, Manipulieren und Durchlaufen des HTML-DOM.

### Verwendung von HtmlAgilityPack

1. **HtmlAgilityPack installieren**: Fügen Sie zunächst das HtmlAgilityPack-Paket Ihrem Projekt über NuGet hinzu.
   ```
   Install-Package HtmlAgilityPack
   ```

2. **Beispielcode**: Parsen Sie einen HTML-String und extrahieren Sie die Titel aller `<h1>`-Elemente.

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

           var h1Tags = htmlDoc.DocumentNode.SelectNodes("//h1").Select(node => node.InnerText);
           foreach (var title in h1Tags)
           {
               Console.WriteLine(title);
           }
       }
   }
   ```

   **Beispielausgabe:**
   ```
   Titel 1
   Titel 2
   ```

### Verwendung von AngleSharp

1. **AngleSharp installieren**: Fügen Sie die AngleSharp-Bibliothek Ihrem Projekt über NuGet hinzu.
   ```
   Install-Package AngleSharp
   ```

2. **Beispielcode**: Laden Sie ein HTML-Dokument und fragen Sie `div`-Elemente mit einer spezifischen Klasse ab.

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

           var items = document.QuerySelectorAll(".item").Select(element => element.TextContent);
           foreach (var item in items)
           {
               Console.WriteLine(item);
           }
       }
   }
   ```

   **Beispielausgabe:**
   ```
   Artikel 1
   Artikel 2
   ```

Sowohl HTMLAgilityPack als auch AngleSharp sind leistungsstarke Werkzeuge zum Parsen von HTML, aber Ihre Wahl zwischen ihnen könnte von spezifischen Projektanforderungen, Leistungsüberlegungen oder persönlichen Vorlieben im API-Design abhängen.
