---
title:                "C#: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

I mange tilfeller kan det være nyttig å kunne laste ned en nettside til en datamaskin for senere bruk. Dette kan være av flere årsaker, for eksempel for å lagre informasjon, analysere data, eller å kunne se på en nettside uten å være koblet til internett.

## Hvordan

Lasting ned en nettside ved hjelp av C# er en relativt enkel prosess. Først må vi inkludere biblioteket "System.Net" i prosjektet vårt. Deretter kan vi bruke "WebClient" klassen til å laste ned en nettside ved å oppgi URL-adressen som en parameter. Se et eksempel under:

```C#
using System;
using System.Net;

namespace LastNettside
{
    class Program
    {
        static void Main(string[] args)
        {
            using (WebClient client = new WebClient())
            {
                string nettside = client.DownloadString("https://www.google.com");
                Console.WriteLine(nettside);
            }
        }
    }
}
```

Koden over vil laste ned nettsiden til Google og skrive ut innholdet på konsollen. Hvis du ønsker å lagre nettsiden som en fil i stedet, kan du bruke "DownloadFile" metoden i stedet for "DownloadString".

## Dypdykk

Det er også mulig å laste ned en nettside og behandle HTML-koden for å hente ut spesifikk informasjon. Dette kan være nyttig for å skrape data fra nettsider eller for å automatisere prosesser som involverer nettsider. Enkleste måten å gjøre dette på, er å bruke et tredjeparts-bibliotek som "HtmlAgilityPack".

For å bruke dette biblioteket, kan du følge de samme stegene som nevnt over for å laste ned en nettside. Deretter kan du bruke "HtmlDocument" klassen for å analysere HTML-koden. Se et eksempel under:

```C#
using System;
using System.Net;
using HtmlAgilityPack;

namespace AnalyserNettside
{
    class Program
    {
        static void Main(string[] args)
        {
            using (WebClient client = new WebClient())
            {
                string nettside = client.DownloadString("https://www.wikipedia.org");
                
                HtmlDocument dokument = new HtmlDocument();
                dokument.LoadHtml(nettside);
                
                // Hent ut tittel-taggen
                HtmlNode tittel = dokument.DocumentNode.SelectSingleNode("//title");
                Console.WriteLine(tittel.InnerText); // Skriver ut "Wikipedia"

                // Hent ut alle lenker på siden
                HtmlNodeCollection lenker = dokument.DocumentNode.SelectNodes("//a[@href]");
                foreach (HtmlNode lenke in lenker)
                {
                    Console.WriteLine(lenke.GetAttributeValue("href", string.Empty));
                }
            }
        }
    }
}
```

Koden over vil laste ned Wikipedia sin nettside, finne tittelen og skrive ut alle lenker på siden. Med litt kunnskap om HTML og XPath kan du hente ut nesten all informasjon du ønsker fra en nettside.

## Se også

- [WebClient klasse (System.Net)](https://docs.microsoft.com/en-us/dotnet/api/system.net.webclient?view=netcore-3.1)
- [HtmlAgilityPack biblioteket](https://html-agility-pack.net/)
- [HTML-tagg referanse](https://www.w3schools.com/tags/default.asp)