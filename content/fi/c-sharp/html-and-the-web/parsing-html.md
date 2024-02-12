---
title:                "HTML:n jäsennys"
aliases:
- /fi/c-sharp/parsing-html.md
date:                  2024-02-03T19:12:09.979123-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML:n jäsennys"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?

HTML:n jäsennys ohjelmoinnissa tarkoittaa HTML-dokumentin rakenteen analysoimista, mikä mahdollistaa sen sisällön ohjelmallisen käsittelyn, muokkaamisen ja vuorovaikutuksen. Ohjelmoijat tekevät tätä automatisoidakseen verkkosivujen kaapimisen, datan poiminnan tai jopa muokatakseen verkkosivuja tai HTML-dokumentteja dynaamisesti eri sovelluksia varten, mikä tekee siitä olennaisen taidon verkkokehityksessä, datan analysoinnissa ja automatisoiduissa testauskuvioissa.

## Kuinka:

Vaikka .NET tarjoaa perustason tuen HTML:n käsittelyyn, kuten `HttpClient`-luokan verkkosivujen noutamiseen, siitä puuttuu sisäänrakennettu, kattava HTML-jäsennin. Siksi useimmat C#-kehittäjät kääntyvät suosittujen kolmansien osapuolien kirjastojen, kuten HtmlAgilityPackin tai AngleSharpin, puoleen vankkojen HTML-jäsennysominaisuuksien saamiseksi. Molemmat kirjastot mahdollistavat HTML DOM:n helpon kyselyn, manipuloinnin ja läpikäynnin.

### HtmlAgilityPackin käyttäminen

1. **Asenna HtmlAgilityPack**: Lisää ensin HtmlAgilityPack-paketti projektiisi NuGetin kautta.
   ```
   Install-Package HtmlAgilityPack
   ```

2. **Esimerkkikoodi**: Jäsennä HTML-merkkijono ja poimi kaikkien `<h1>`-elementtien otsikot.

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
                             <h1>Title 1</h1>
                             <h1>Title 2</h1>
                         </body>
                        </html>";
           var htmlDoc = new HtmlDocument();
           htmlDoc.LoadHtml(html);

           var h1Tags = htmlDoc.DocumentNode.SelectNodes("//h1").Select(node => node.InnerText);
           foreach (var otsikko in h1Tags)
           {
               Console.WriteLine(otsikko);
           }
       }
   }
   ```

   **Esimerkkituloste:**
   ```
   Otsikko 1
   Otsikko 2
   ```

### AngleSharpin käyttäminen

1. **Asenna AngleSharp**: Lisää AngleSharp-kirjasto projektiisi NuGetin kautta.
   ```
   Install-Package AngleSharp
   ```

2. **Esimerkkikoodi**: Lataa HTML-dokumentti ja kysely `div`-elementit, joilla on tietty luokka.

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
           var document = await context.OpenAsync(req => req.Content("<div class='item'>Esine 1</div><div class='item'>Esine 2</div>"));

           var items = document.QuerySelectorAll(".item").Select(element => element.TextContent);
           foreach (var esine in items)
           {
               Console.WriteLine(esine);
           }
       }
   }
   ```

   **Esimerkkituloste:**
   ```
   Esine 1
   Esine 2
   ```

Sekä HTMLAgilityPack että AngleSharp ovat tehokkaita työkaluja HTML:n jäsennykseen, mutta valintasi niiden välillä saattaa riippua tietystä projektin vaatimuksista, suorituskykyä koskevista harkinnoista tai henkilökohtaisesta mieltymyksestä API-suunnittelussa.
