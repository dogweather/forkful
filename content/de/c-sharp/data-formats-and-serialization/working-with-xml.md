---
date: 2024-01-26 04:28:42.748220-07:00
description: "Wie geht das: XML gibt es schon seit Ende der 90er Jahre, was es in\
  \ Tech-Jahren zum Gro\xDFvater macht. Es wurde f\xFCr die Datenportabilit\xE4t und\
  \ die einfache\u2026"
lastmod: '2024-04-05T21:53:55.796454-06:00'
model: gpt-4-0125-preview
summary: "XML gibt es schon seit Ende der 90er Jahre, was es in Tech-Jahren zum Gro\xDF\
  vater macht."
title: Arbeiten mit XML
weight: 40
---

## Wie geht das:
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

        // Den String in ein XDocument parsen
        XDocument doc = XDocument.Parse(xmlString);

        // Ein neues Buch hinzufügen
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Learning XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // Das XML auf der Konsole ausgeben
        Console.WriteLine(doc);

        // Das Dokument laden
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // Alle Preise abrufen
        XmlNodeList prices = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode price in prices)
        {
            Console.WriteLine(price.InnerText);
        }
     }
}

// Beispielausgabe:
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

## Tiefer Eintauchen
XML gibt es schon seit Ende der 90er Jahre, was es in Tech-Jahren zum Großvater macht. Es wurde für die Datenportabilität und die einfache Lesbarkeit durch Menschen erdacht. Alternativen wie JSON sind ihm mittlerweile auf den Fersen, besonders in Web-Kontexten, da es leichter und für viele einfacher zu handhaben ist. Aber XML hält sich immer noch in zahlreichen Alt-Systemen und bestimmten Kommunikationsprotokollen. Mit XML erhalten Sie ein Schema zur Validierung Ihrer Struktur und Namensräume, um Tag-Konflikte zu vermeiden – Funktionen, die von seiner unternehmensbereiten Reife zeugen.

In C# sind die `System.Xml.Linq` und `System.Xml` Namespaces zwei große Hilfsmittel, um mit XML zu arbeiten. LINQ to XML (`XDocument`, `XElement`) ist moderner und eleganter – Sie haben seine Magie im Beispiel gesehen. `XmlDocument` bietet Ihnen den DOM (Document Object Model)-Ansatz – ein bisschen altmodisch, aber einige schwören auf seine Kraft.

## Siehe auch
- [MSDN – Übersicht über LINQ to XML](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – XML Document Object Model (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – XML lernen](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/xml.html)
