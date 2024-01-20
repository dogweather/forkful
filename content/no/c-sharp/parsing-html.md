---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å parse HTML er prosessen med å oversette HTML-tekst til et format som et program kan forstå, som et dokumentobjektmodell. Programmerere gjør dette for å hente nyttige data fra websider eller manipulere HTML-dokumenter.

## Hvordan gjøre det:
For å parse HTML i C#, kan vi bruke HtmlAgilityPack, et populært .NET-bibliotek. Her er en enkel kodebit som viser hvordan vi kan hente alle hyperlinkene fra en nettside:

```C#
using System;
using HtmlAgilityPack;
 
public class Program
{
    public static void Main()
    {
        var url = "https://eksempel.com";
        var web = new HtmlWeb();
        var htmlDoc = web.Load(url);

        var node = htmlDoc.DocumentNode.SelectNodes("//a[@href]");

        foreach(var link in node)
        {
            Console.WriteLine(link.GetAttributeValue("href", ""));
        }
    }
}
```

## Dypdykk
Historisk sett ble HTML-parsing i C# påbegynt med biblioteker som HtmlAgilityPack, fordi det tilbyr funksjonalitet som ikke er tilgjengelig i standardbiblioteket. Alternativer inkluderer AngleSharp.

Å parse HTML krever vanligvis at du først laster inn HTML-siden du vil parse, deretter bruker du en metode for å søke gjennom nodene på siden. Du kan søke etter bestemte noder ved å bruke XPath-uttrykk, CSS-selektorer eller klasse- og id-attributter.

## Se også:
1. [HtmlAgilityPack dokumentasjon](https://html-agility-pack.net/)
2. [AngleSharp: En alternativ HTML-parser i C#](https://anglesharp.github.io/)
4. [Offisielle HTML5 Spesifikasjoner](https://www.w3.org/TR/html52/)