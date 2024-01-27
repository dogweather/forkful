---
title:                "Trabajando con XML"
date:                  2024-01-26T04:29:21.385615-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con XML"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/working-with-xml.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
XML (Lenguaje de Marcas eXtensible) trata sobre estructurar datos en un formato legible. Los programadores manejan XML para configuración, intercambio de datos entre aplicaciones y donde las especificaciones lo requieran—piense en SOAP o APIs web.

## Cómo hacerlo:
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

        // Analiza la cadena en un XDocument
        XDocument doc = XDocument.Parse(xmlString);

        // Agregar un nuevo libro
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Aprendiendo XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // Escribir el XML en la consola
        Console.WriteLine(doc);

        // Cargar el documento
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // Recuperar todos los precios
        XmlNodeList precios = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode precio in precios)
        {
            Console.WriteLine(precio.InnerText);
        }
     }
}

// Salida de Muestra:
// <bookstore>
//  <book>
//    <title lang="en">Head First C#</title>
//    <price>39.99</price>
//  </book>
//  <book>
//    <title lang="en">Aprendiendo XML</title>
//    <price>29.99</price>
//  </book>
// </bookstore>
// 39.99
// 29.99
```

## Análisis Profundo
XML existe desde fines de los '90, lo que lo convierte en un abuelo en años tecnológicos. Fue ideado para la portabilidad de datos y facilidad de lectura humana. Alternativas como JSON ahora le están pisando los talones, especialmente en contextos web, ya que es más ligero y, para muchos, más sencillo de manejar. Pero XML aún mantiene su posición en numerosos sistemas legados y ciertos protocolos de comunicación. Con XML, obtienes un esquema para validar tu estructura y espacios de nombres para evitar conflictos de etiquetas—características que hablan de su madurez lista para empresas.

En C#, los espacios de nombres `System.Xml.Linq` y `System.Xml` son dos grandes herramientas para trabajar con XML. LINQ to XML (`XDocument`, `XElement`) es más moderno y más elegante—has visto su magia en el ejemplo. `XmlDocument` te ofrece el enfoque DOM (Modelo de Objeto de Documento)—un poco anticuado, pero algunos juran por su poder.

## Ver También
- [MSDN – Visión General de LINQ a XML](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – Modelo de Objeto de Documento XML (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – Aprender XML](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/xml.html)