---
title:                "Tolka HTML"
date:                  2024-01-20T15:30:44.788950-07:00
html_title:           "Arduino: Tolka HTML"
simple_title:         "Tolka HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att parsa HTML innebär att man analyserar HTML-kod för att plocka ut specifik data eller strukturer. Programmerare gör detta för att kunna interagera med webbsidor, extrahera information eller automatisera webb-baserade uppgifter.

## Hur man gör:
I C# kan vi använda HtmlAgilityPack för att parsa HTML enkelt. Här är ett grunt dyk med exempel:

```C#
// Installera HtmlAgilityPack med NuGet
// Install-Package HtmlAgilityPack

using HtmlAgilityPack;

var html = @"<!DOCTYPE html>
<html>
<body>
    <h1>Hej Sverige!</h1>
    <p>Det här är ett paragraf.</p>
</body>
</html>";

var htmlDoc = new HtmlDocument();
htmlDoc.LoadHtml(html);

var headerNode = htmlDoc.DocumentNode.SelectSingleNode("//h1");
var paragraphNode = htmlDoc.DocumentNode.SelectSingleNode("//p");

Console.WriteLine(headerNode.InnerText); // Skriver ut: Hej Sverige!
Console.WriteLine(paragraphNode.InnerText); // Skriver ut: Det här är ett paragraf.
```

## Fördjupning
Parsing av HTML är inte nytt. En av de äldsta metoderna är att använda reguljära uttryck, men det är opraktiskt och skört för komplex HTML. HtmlAgilityPack är ett C# bibliotek som har använts sedan början av 2000-talet. Det fungerar genom att skapa en DOM-trädstruktur av HTML-koden som kan navigeras och manipuleras.

Alternativ till HtmlAgilityPack inkluderar AngleSharp, en modernare bibliotek som följer de senaste webbstandarderna.

I termer av utförande använder HtmlAgilityPack XPath- och Linq-querys för att hämta specifika noder, vilket är kraftfullt men kräver förståelse för frågespråken.

## Se också
- HtmlAgilityPack GitHub-repo: https://github.com/zzzprojects/html-agility-pack
- AngleSharp GitHub-repo: https://github.com/AngleSharp/AngleSharp
- XPath tutorial: https://www.w3schools.com/xml/xpath_intro.asp
- LINQ to XML-dokumentation: https://docs.microsoft.com/en-us/dotnet/standard/linq/linq-xml-overview
