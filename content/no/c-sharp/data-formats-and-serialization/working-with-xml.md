---
title:                "Å jobbe med XML"
aliases:
- /no/c-sharp/working-with-xml.md
date:                  2024-01-26T04:28:56.214925-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/working-with-xml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
XML (eXtensible Markup Language) handler om å strukturere data i et lesbart format. Programmerere jobber med XML for konfigurasjon, datautveksling mellom apper, og der spesifikasjoner ber om det—tenk SOAP eller web-APIer.

## Hvordan:
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

        // Tolker strengen til et XDocument
        XDocument doc = XDocument.Parse(xmlString);

        // Legger til en ny bok
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Learning XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // Skriver XML til konsollen
        Console.WriteLine(doc);

        // Laster dokumentet
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // Henter alle priser
        XmlNodeList prices = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode price in prices)
        {
            Console.WriteLine(price.InnerText);
        }
     }
}

// Eksempel på utskrift:
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

## Dypdykk
XML har eksistert siden slutten av 90-tallet, noe som gjør det til en bestefar i teknologiårene. Det ble skapt for dataoverførbarhet og letthet i lesbarhet for mennesker. Alternativer som JSON er nå tett i hælene, spesielt i webkontekster, fordi det er lettere og, for mange, enklere å håndtere. Men XML holder fortsatt sin posisjon i mange eldre systemer og visse kommunikasjonsprotokoller. Med XML får du et skjema for å validere strukturen din og navneområder for å unngå tag-konflikter—funksjoner som taler for dets modenhet klar for bedriftsbruk.

I C#, er `System.Xml.Linq` og `System.Xml` navneområder to store verktøy for å jobbe med XML. LINQ til XML (`XDocument`, `XElement`) er mer moderne og mer elegant—du har sett magien i eksemplet. `XmlDocument` gir deg DOM (Document Object Model)-tilnærmingen—litt gammeldags, men noen sverger til dens kraft.

## Se også
- [MSDN – Oversikt over LINQ til XML](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – XML Document Object Model (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – Lær XML](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/xml.html)
