---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:00.177256-07:00
description: "L'analisi dell'HTML nella programmazione consiste nell'analizzare la\
  \ struttura di un documento HTML, permettendo di estrarre, manipolare e interagire\
  \ con\u2026"
lastmod: '2024-03-13T22:44:43.431728-06:00'
model: gpt-4-0125-preview
summary: L'analisi dell'HTML nella programmazione consiste nell'analizzare la struttura
  di un documento HTML, permettendo di estrarre, manipolare e interagire con il suo
  contenuto in modo programmatico.
title: Analisi del HTML
weight: 43
---

## Come fare:
Sebbene .NET fornisca un supporto di base per lavorare con l'HTML, come l'`HttpClient` per il recupero di pagine web, manca di un parser HTML integrato e completo. Pertanto, la maggior parte dei sviluppatori C# si rivolge a librerie di terze parti popolari come HtmlAgilityPack o AngleSharp per funzionalità di parsing HTML robuste. Entrambe le librerie permettono una facile interrogazione, manipolazione e traversata del DOM HTML.

### Utilizzando HtmlAgilityPack
1. **Installa HtmlAgilityPack**: Prima, aggiungi il pacchetto HtmlAgilityPack al tuo progetto tramite NuGet.
   ```
   Install-Package HtmlAgilityPack
   ```

2. **Esempio di Codice**: Analizza una stringa HTML ed estrai i titoli di tutti gli elementi `<h1>`.

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
                             <h1>Titolo 1</h1>
                             <h1>Titolo 2</h1>
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

   **Output dell'Esempio:**
   ```
   Titolo 1
   Titolo 2
   ```

### Utilizzando AngleSharp
1. **Installa AngleSharp**: Aggiungi la libreria AngleSharp al tuo progetto tramite NuGet.
   ```
   Install-Package AngleSharp
   ```

2. **Esempio di Codice**: Carica un documento HTML e interroga gli elementi `div` con una classe specifica.

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
           var document = await context.OpenAsync(req => req.Content("<div class='item'>Elemento 1</div><div class='item'>Elemento 2</div>"));

           var items = document.QuerySelectorAll(".item").Select(element => element.TextContent);
           foreach (var item in items)
           {
               Console.WriteLine(item);
           }
       }
   }
   ```

   **Output dell'Esempio:**
   ```
   Elemento 1
   Elemento 2
   ```

Sia HTMLAgilityPack che AngleSharp sono strumenti potenti per l'analisi dell'HTML, ma la tua scelta tra loro potrebbe dipendere da requisiti specifici del progetto, considerazioni sulle prestazioni o preferenze personali nella progettazione dell'API.
