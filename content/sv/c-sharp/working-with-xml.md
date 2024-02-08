---
title:                "Att arbeta med XML"
aliases:
- sv/c-sharp/working-with-xml.md
date:                  2024-01-26T04:29:10.377899-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/working-with-xml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
XML (eXtensible Markup Language) handlar om att strukturera data i ett läsbart format. Programmerare jonglerar med XML för konfiguration, datautbyte mellan appar och där specifikationer kräver det—tänk SOAP eller webb-APIer.

## Hur man gör:
```C#
using System;
using System.Xml;
using System.Xml.Linq;

class Program
{
     static void Main()
     {
        var xmlString = @"<bookstore>
                            <book>
                              <title lang=""en"">Head First C#</title>
                              <price>39.99</price>
                            </book>
                          </bookstore>";

        // Tolka strängen till ett XDocument
        XDocument doc = XDocument.Parse(xmlString);

        // Lägg till en ny bok
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Learning XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // Skriv ut XML till konsolen
        Console.WriteLine(doc);

        // Ladda dokumentet
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // Hämta alla priser
        XmlNodeList prices = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode price in prices)
        {
            Console.WriteLine(price.InnerText);
        }
     }
}

// Exempelutdata:
// <bookstore>
//  <book>
//    <title lang="en">Head First C#</title>
//    <price>39.99</price>
//  </book>
//  <book>
//    <title lang="en">Learning XML</title>
//    <price>29.99</price>
//  </book>
// </bookstore>
// 39.99
// 29.99
```

## Djupdykning
XML har funnits sedan sent '90-tal, vilket gör det till en farfar i teknologiår. Det trollades fram för portabilitet av data och läslighet för människor. Alternativ som JSON nafsar nu på dess klackar, särskilt i webbsammanhang, eftersom det är lättare och, för många, enklare att hantera. Men XML håller fortfarande stånd i många äldre system och vissa kommunikationsprotokoll. Med XML får du ett schema för att validera din struktur och namnrymder för att undvika taggkonflikter—funktioner som talar för dess företagsklara mognad.

I C#, är `System.Xml.Linq` och `System.Xml` namnrymder två stora verktyg för att arbeta med XML. LINQ till XML (`XDocument`, `XElement`) är modernare och mer elegant—du såg dess magi i exemplet. `XmlDocument` ger dig DOM (Document Object Model) tillvägagångssättet - lite gammaldags, men vissa svär vid dess kraft.

## Se även
- [MSDN – LINQ till XML Översikt](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – XML Dokument Objekt Modellen (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – Lär dig XML](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/xml.html)
