---
date: 2024-01-26 04:28:59.620620-07:00
description: "XML (eXtensible Markup Language) riguarda la strutturazione dei dati\
  \ in un formato leggibile. I programmatori maneggiano XML per configurazione, scambio\u2026"
lastmod: 2024-02-19 22:05:02.522645
model: gpt-4-0125-preview
summary: "XML (eXtensible Markup Language) riguarda la strutturazione dei dati in\
  \ un formato leggibile. I programmatori maneggiano XML per configurazione, scambio\u2026"
title: Lavorare con XML
---

{{< edit_this_page >}}

## Cosa e Perché?
XML (eXtensible Markup Language) riguarda la strutturazione dei dati in un formato leggibile. I programmatori maneggiano XML per configurazione, scambio di dati tra app e dove le specifiche lo richiedono—pensa a SOAP o alle API web.

## Come fare:
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

        // Analizza la stringa in un XDocument
        XDocument doc = XDocument.Parse(xmlString);

        // Aggiungi un nuovo libro
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Imparare XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // Scrivi l'XML sulla console
        Console.WriteLine(doc);

        // Carica il documento
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // Recupera tutti i prezzi
        XmlNodeList prices = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode price in prices)
        {
            Console.WriteLine(price.InnerText);
        }
     }
}

// Output di esempio:
// <bookstore>
//  <book>
//    <title lang="en">Head First C#</title>
//    <price>39.99</price>
//  </book>
//  <book>
//    <title lang="en">Imparare XML</title>
//    <price>29.99</price>
//  </book>
// </bookstore>
// 39.99
// 29.99
```

## Approfondimento
XML esiste fin dagli anni '90, rendendolo un nonno negli anni tecnologici. È stato concepito per la portabilità dei dati e la facilità di lettura umana. Alternative come JSON stanno ora rosicchiando il suo terreno, specialmente nei contesti web, perché è più leggero e, per molti, più semplice da gestire. Ma XML mantiene ancora la sua posizione in numerosi sistemi legacy e certi protocolli di comunicazione. Con XML, ottieni uno schema per validare la tua struttura e namespace per evitare conflitti di tag—funzionalità che parlano della sua maturità pronta per l'impresa.

In C#, `System.Xml.Linq` e `System.Xml` sono due grandi risorse per lavorare con XML. LINQ to XML (`XDocument`, `XElement`) è più moderno ed elegante—hai visto la sua magia nell'esempio. `XmlDocument` ti offre l'approccio DOM (Document Object Model)—un po' vecchia scuola, ma alcuni lo giurano per il suo potere.

## Vedi Anche
- [MSDN – Panoramica di LINQ to XML](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – XML Document Object Model (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – Impara XML](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/xml.html)
