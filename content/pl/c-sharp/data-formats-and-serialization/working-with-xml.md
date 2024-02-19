---
aliases:
- /pl/c-sharp/working-with-xml/
date: 2024-01-26 04:28:58.945406-07:00
description: "XML (eXtensible Markup Language) dotyczy strukturyzowania danych w czytelnym\
  \ formacie. Programi\u015Bci \u017Congluj\u0105 XML-em do konfiguracji, wymiany\
  \ danych mi\u0119dzy\u2026"
lastmod: 2024-02-18 23:08:49.636226
model: gpt-4-0125-preview
summary: "XML (eXtensible Markup Language) dotyczy strukturyzowania danych w czytelnym\
  \ formacie. Programi\u015Bci \u017Congluj\u0105 XML-em do konfiguracji, wymiany\
  \ danych mi\u0119dzy\u2026"
title: Praca z XML
---

{{< edit_this_page >}}

## Co i dlaczego?
XML (eXtensible Markup Language) dotyczy strukturyzowania danych w czytelnym formacie. Programiści żonglują XML-em do konfiguracji, wymiany danych między aplikacjami oraz tam, gdzie specyfikacje tego wymagają—pomyśl o SOAP czy web API.

## Jak to zrobić:
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

        // Analiza ciągu znaków na XDocument
        XDocument doc = XDocument.Parse(xmlString);

        // Dodaj nową książkę
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Learning XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // Wypisz XML na konsolę
        Console.WriteLine(doc);

        // Załaduj dokument
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // Pobierz wszystkie ceny
        XmlNodeList prices = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode price in prices)
        {
            Console.WriteLine(price.InnerText);
        }
     }
}

// Przykładowy wynik:
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

## Wgłębienie
XML istnieje od końca lat '90, co czyni go dziadkiem w latach technologicznych. Został wymyślony dla przenośności danych i łatwości odczytu przez ludzi. Alternatywy takie jak JSON teraz depczą mu po piętach, zwłaszcza w kontekstach sieciowych, ponieważ jest lżejszy i, dla wielu, prostszy w obsłudze. Ale XML nadal trzyma się mocno w wielu starszych systemach i pewnych protokołach komunikacyjnych. Z XML-em otrzymujesz schemat do walidacji swojej struktury i przestrzenie nazw, aby uniknąć konfliktów tagów—cechy, które mówią o jego gotowości na wykorzystanie w przedsiębiorstwach.

W C#, `System.Xml.Linq` i `System.Xml` to dwie duże przestrzenie nazw do pracy z XML-em. LINQ do XML (`XDocument`, `XElement`) jest bardziej nowoczesne i eleganckie—widzieliście jego magię w przykładzie. `XmlDocument` daje ci podejście DOM (Document Object Model)—trochę staroszkolne, ale niektórzy przysięgają na jego moc.

## Zobacz także
- [MSDN – Przegląd LINQ do XML](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – Model Obiektowy Dokumentu XML (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – Ucz się XML](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/xml.html)
