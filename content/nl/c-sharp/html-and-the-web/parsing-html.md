---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:30.468890-07:00
description: "HTML parseren betekent het extraheren van informatie uit HTML-documenten.\
  \ Programmeurs doen dit om programmatisch met webinhoud om te gaan, gegevens te\u2026"
lastmod: '2024-02-25T18:49:48.147433-07:00'
model: gpt-4-0125-preview
summary: "HTML parseren betekent het extraheren van informatie uit HTML-documenten.\
  \ Programmeurs doen dit om programmatisch met webinhoud om te gaan, gegevens te\u2026"
title: HTML Parsen
---

{{< edit_this_page >}}

## Wat & Waarom?
HTML parseren betekent het extraheren van informatie uit HTML-documenten. Programmeurs doen dit om programmatisch met webinhoud om te gaan, gegevens te schrapen of webinteracties te automatiseren.

## Hoe:
Laten we een populaire .NET-bibliotheek voor HTML-parsing gebruiken: HtmlAgilityPack.

Eerst, installeer het via NuGet:
```shell
Install-Package HtmlAgilityPack
```

Vervolgens, laad een HTML-document en pak wat knooppunten:

```C#
using System;
using HtmlAgilityPack;

class Program
{
    static void Main()
    {
        var web = new HtmlWeb();
        var doc = web.Load("http://example.com");

        foreach (var node in doc.DocumentNode.SelectNodes("//a[@href]"))
        {
            Console.WriteLine($"Tekst: {node.InnerText}, Link: {node.Attributes["href"].Value}");
        }
    }
}
```
Het bovenstaande fragment haalt alle anker tags op met een `href` attribuut en print hun tekst en link.

Een voorbeelduitvoer kan er als volgt uitzien:

```
Tekst: Home, Link: http://example.com/home
Tekst: Over, Link: http://example.com/about
...
```

## Diepgaand
HtmlAgilityPack (HAP) is sinds de vroege jaren 2000 toonaangevend voor parsing. Het is geliefd vanwege zijn flexibiliteit en gebruiksgemak, en het lijkt sterk op de DOM in browsers.

Alternatieven? Zeker. AngleSharp is een nieuwere bibliotheek, met ondersteuning voor asynchroon en volgt de huidige webstandaarden nauwer. Voor eenvoudige taken zou je zelfs Regex kunnen gebruiken, maar wees gewaarschuwd - HTML is niet gemaakt om regex-vriendelijk te zijn. Het is op zijn best een hacky oplossing.

Wat betreft de implementatie, HAP parseert de gegeven HTML naar een DOM-achtige structuur, waardoor je knooppunten kunt opvragen en manipuleren met XPath of LINQ. Het is robuust genoeg om ongelijkmatige HTML aan te kunnen, wat het een voordeel geeft bij het scrapen van echte, vaak imperfecte webpagina's.

## Zie Ook
- HtmlAgilityPack op GitHub: [https://github.com/zzzprojects/html-agility-pack](https://github.com/zzzprojects/html-agility-pack)
- AngleSharp GitHub & documentatie: [https://github.com/AngleSharp/AngleSharp](https://github.com/AngleSharp/AngleSharp)
- Artikel over beste praktijken voor webscraping: (link naar een betrouwbare bron met richtlijnen en de legaliteit van webscraping).
