---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:30:43.138755-07:00
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
HTML-parsinta tarkoittaa HTML-dokumentin rakenteen lukemista ja sen datan muuttamista käsiteltäväksi muotoon. Ohjelmoijat parsivat HTML:ää esimerkiksi tiedon raapimiseksi, sisällön muokkaamiseksi tai web-sovellusten testaamiseksi.

## How to: (Kuinka:)
Käytetään HtmlAgilityPack-kirjastoa, jonka saa NuGet-paketinhallinnasta. Tässä on esimerkki HTML-dokumentin lataamisesta ja linkkien tulostamisesta.

```C#
using HtmlAgilityPack;
using System;
using System.Linq;

class Program
{
    static void Main()
    {
        var web = new HtmlWeb();
        var doc = web.Load("https://esimerkki.fi");

        var linkit = doc.DocumentNode.SelectNodes("//a[@href]")
                        .Select(a => a.Attributes["href"].Value)
                        .ToList();

        linkit.ForEach(Console.WriteLine);
    }
}
```

Tämän pitäisi tulostaa kaikki `esimerkki.fi` sivun linkit.

## Deep Dive (Syväsukellus):
HTML-parsinta on 90-luvulta asti ollut tarpeen, kun tiedon jakaminen webissä yleistyi. Päävaihtoehdot ovat regex (sopii huonosti HTML:n monimuotoisuuteen) ja DOM-pohjaiset parserit (kuten esimerkin HtmlAgilityPack). DOM-parserit luovat helposti käsiteltävän puurakenteen, jolloin tiedon erottaminen ja manipulaatio on yksinkertaista. Performanssi ja tarkkuus ovat avaintekijöitä parseria valittaessa.

## See Also (Katso Myös):
- HtmlAgilityPack: [https://html-agility-pack.net/](https://html-agility-pack.net/)
- C# dokumentaatio: [https://docs.microsoft.com/fi-fi/dotnet/csharp/](https://docs.microsoft.com/fi-fi/dotnet/csharp/)
- XPath tutorial: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
