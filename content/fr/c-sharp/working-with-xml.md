---
title:                "Travailler avec XML"
date:                  2024-01-26T04:28:40.575541-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec XML"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/working-with-xml.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
XML (eXtensible Markup Language) concerne la structuration des données dans un format lisible. Les programmeurs manipulent du XML pour la configuration, l'échange de données entre applications, et là où les spécifications le demandent - pensez à SOAP ou aux API web.

## Comment faire :
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

        // Analyser la chaîne en un XDocument
        XDocument doc = XDocument.Parse(xmlString);

        // Ajouter un nouveau livre
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Apprendre XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // Écrire le XML dans la console
        Console.WriteLine(doc);

        // Charger le document
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // Récupérer tous les prix
        XmlNodeList prices = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode price in prices)
        {
            Console.WriteLine(price.InnerText);
        }
     }
}

// Exemple de sortie :
// <bookstore>
//  <book>
//    <title lang="en">Head First C#</title>
//    <price>39.99</price>
//  </book>
//  <book>
//    <title lang="en">Apprendre XML</title>
//    <price>29.99</price>
//  </book>
// </bookstore>
// 39.99
// 29.99
```

## Plongée en profondeur
XML existe depuis la fin des années 90, ce qui en fait un grand-père en années technologiques. Il a été inventé pour la portabilité des données et la facilité de lecture humaine. D'autres alternatives comme JSON lui mordent désormais les talons, surtout dans les contextes web, car il est plus léger et, pour beaucoup, plus simple à manipuler. Mais XML tient toujours sa place dans de nombreux systèmes hérités et certains protocoles de communication. Avec XML, vous obtenez un schéma pour valider votre structure et des espaces de noms pour éviter les conflits de balises - des caractéristiques qui parlent de sa maturité prête pour l'entreprise.

En C#, les espaces de noms `System.Xml.Linq` et `System.Xml` sont deux gros canons pour travailler avec XML. LINQ to XML (`XDocument`, `XElement`) est plus moderne et plus élégant - vous avez vu sa magie dans l'exemple. `XmlDocument` vous donne l'approche DOM (Document Object Model) - un peu à l'ancienne, mais certains jurent par sa puissance.

## Voir aussi
- [MSDN – Aperçu de LINQ to XML](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – Modèle d'objet de document XML (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – Apprendre XML](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/xml.html)
