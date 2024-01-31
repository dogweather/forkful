---
title:                "Werken met XML"
date:                  2024-01-28T22:11:27.421033-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met XML"

category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c-sharp/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
XML (eXtensible Markup Language) gaat over het structureren van data in een leesbaar formaat. Programmeurs werken met XML voor configuratie, gegevensuitwisseling tussen apps, en waar specificaties erom vragen—denk aan SOAP of web-API's.

## Hoe:
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

        // Parseer de string naar een XDocument
        XDocument doc = XDocument.Parse(xmlString);

        // Voeg een nieuw boek toe
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Leer XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // Schrijf de XML naar de console
        Console.WriteLine(doc);

        // Laad het document
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // Haal alle prijzen op
        XmlNodeList prijzen = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode prijs in prijzen)
        {
            Console.WriteLine(prijs.InnerText);
        }
     }
}

// Voorbeelduitvoer:
// <bookstore>
//  <book>
//    <title lang="en">Head First C#</title>
//    <price>39.99</price>
//  </book>
//  <book>
//    <title lang="en">Leer XML</title>
//    <price>29.99</price>
//  </book>
// </bookstore>
// 39.99
// 29.99
```

## Diepgaande duik
XML is er al sinds de late jaren '90, waardoor het een opa is in technologiejaren. Het is bedacht voor draagbaarheid van gegevens en gemak van menselijk lezen. Alternatieven zoals JSON nemen nu het stokje over, vooral in webcontexten, omdat het lichter is en voor velen eenvoudiger te hanteren. Maar XML houdt nog steeds zijn positie vast in talrijke legacy-systemen en bepaalde communicatieprotocollen. Met XML krijg je een schema om je structuur te valideren en namespaces om tag-conflicten te vermijden—functies die spreken van zijn bedrijfsklare volwassenheid.

In C# zijn `System.Xml.Linq` en `System.Xml` twee grote kanonnen om met XML te werken. LINQ to XML (`XDocument`, `XElement`) is moderner en eleganter—je hebt de magie ervan gezien in het voorbeeld. `XmlDocument` geeft je de DOM (Document Object Model) benadering—een beetje ouderwets, maar sommigen zweren bij zijn kracht.

## Zie ook
- [MSDN – LINQ to XML Overzicht](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – XML Document Object Model (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – Leer XML](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/xml.html)
