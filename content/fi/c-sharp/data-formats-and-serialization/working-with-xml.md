---
date: 2024-01-26 04:28:57.506935-07:00
description: "Kuinka: XML on ollut olemassa 90-luvun lopulta l\xE4htien, tehden siit\xE4\
  \ suorastaan isois\xE4n teknologiavuosissa. Se kehitettiin datan siirrett\xE4vyyden\
  \ ja\u2026"
lastmod: '2024-04-05T21:53:58.165624-06:00'
model: gpt-4-0125-preview
summary: "XML on ollut olemassa 90-luvun lopulta l\xE4htien, tehden siit\xE4 suorastaan\
  \ isois\xE4n teknologiavuosissa."
title: "XML:n k\xE4sittely"
weight: 40
---

## Kuinka:
```C#
using System;
using System.Xml;
using System.Xml.Linq;

class Program
{
     static void Main()
     {
        var xmlMerkkijono = @"<bookstore>
                            <book>
                              <title lang=""en"">Head First C#</title>
                              <price>39.99</price>
                            </book>
                          </bookstore>";

        // Jäsennä merkkijono XDocumentiksi
        XDocument doc = XDocument.Parse(xmlMerkkijono);

        // Lisää uusi kirja
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Learning XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // Kirjoita XML konsoliin
        Console.WriteLine(doc);

        // Lataa asiakirja
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlMerkkijono);

        // Hae kaikki hinnat
        XmlNodeList hinnat = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode hinta in hinnat)
        {
            Console.WriteLine(hinta.InnerText);
        }
     }
}

// Esimerkkituloste:
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

## Syväsukellus
XML on ollut olemassa 90-luvun lopulta lähtien, tehden siitä suorastaan isoisän teknologiavuosissa. Se kehitettiin datan siirrettävyyden ja ihmisen luettavuuden helpottamiseksi. Vaihtoehtoiset formaatit kuten JSON ovat nyt sen kintereillä, erityisesti web-yhteyksissä, koska se on kevyempi ja monien mielestä yksinkertaisempi käsitellä. Mutta XML pitää yhä paikkansa monissa legacy-järjestelmissä ja tietyissä viestintäprotokollissa. XML:n kanssa saat skeeman rakenteen validoimiseen ja nimiavaruudet tagejen päällekkäisyyksien välttämiseksi – ominaisuuksia, jotka puhuvat sen yritysvalmiista kypsyydestä.

C#:ssa `System.Xml.Linq` ja `System.Xml` nimiavaruudet ovat kaksi suurta työkalua XML:n kanssa toimimiseen. LINQ to XML (`XDocument`, `XElement`) on modernimpi ja elegantimpi – olet nähnyt sen taikuuden esimerkissä. `XmlDocument` antaa sinulle DOM (Document Object Model) lähestymistavan – hieman vanhanaikainen, mutta jotkut vannovat sen voiman nimeen.

## Katso myös
- [MSDN – LINQ to XML Yleiskatsaus](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – XML Document Object Model (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – Opi XML](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/xml.html)
