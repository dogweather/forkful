---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:09.575384-07:00
description: "Het downloaden van een webpagina betekent het ophalen van de ruwe HTML-inhoud\
  \ van het internet met behulp van code. Programmeurs doen dit om gegevens te\u2026"
lastmod: '2024-03-13T22:44:50.808742-06:00'
model: gpt-4-0125-preview
summary: "Het downloaden van een webpagina betekent het ophalen van de ruwe HTML-inhoud\
  \ van het internet met behulp van code. Programmeurs doen dit om gegevens te\u2026"
title: Een webpagina downloaden
---

{{< edit_this_page >}}

## Wat & Waarom?

Het downloaden van een webpagina betekent het ophalen van de ruwe HTML-inhoud van het internet met behulp van code. Programmeurs doen dit om gegevens te verwerken, interactie met webservices uit te voeren, of simpelweg informatie op te slaan voor offline gebruik.

## Hoe te:

C# maakt het eenvoudig om een webpagina te downloaden met de `HttpClient` klasse. Hier volgt een snel voorbeeld:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        using (HttpClient client = new HttpClient())
        {
            try
            {
                string url = "http://example.com"; // Vervang door de gewenste URL
                HttpResponseMessage response = await client.GetAsync(url);
                response.EnsureSuccessStatusCode();
                string responseBody = await response.Content.ReadAsStringAsync();
                
                Console.WriteLine(responseBody); // Geeft de ruwe HTML-inhoud weer
            }
            catch (HttpRequestException e)
            {
                Console.WriteLine("\nUitzondering Opgelopen!");
                Console.WriteLine("Bericht :{0} ", e.Message);
            }
        }
    }
}
```

Dit zal de HTML-inhoud van de opgegeven webpagina naar de console uitvoeren.

## Diepere Duik

Vóór `HttpClient`, gebruikte C# klassen zoals `WebClient` en `HttpWebRequest` om webinhoud te downloaden. `HttpClient` is de nieuwste en is ontworpen om herbruikbaar, efficiënt te zijn, en ondersteunt asynchrone bewerkingen, wat het de voorkeurkeuze maakt voor nieuwe applicaties.

Er bestaan alternatieven. Zo kunnen bijvoorbeeld externe bibliotheken zoals `HtmlAgilityPack` HTML analyseren, wat het gemakkelijker maakt om door de DOM te navigeren of specifieke stukjes informatie te extraheren zonder te hoeven omgaan met rauwe HTML-strings.

Bij het downloaden van webpagina's, vergeet niet: respecteer robots.txt-bestanden, handel uitzonderingen af, en wees bewust van de gebruiksvoorwaarden van websites.

## Zie Ook

- [HttpClient Klasse Documentatie](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Asynchroon en Wachten](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/async/)
- [HTML Agility Pack op GitHub](https://github.com/zzzprojects/html-agility-pack)
- [Respecteren van robots.txt](https://developers.google.com/search/docs/advanced/robots/intro)
