---
title:                "Trabalhando com XML"
aliases:
- pt/c-sharp/working-with-xml.md
date:                  2024-01-26T04:29:35.147376-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/working-with-xml.md"
---

{{< edit_this_page >}}

## O que é & Por quê?
XML (eXtensible Markup Language) trata-se de estruturar dados em um formato legível. Programadores manipulam o XML para configuração, troca de dados entre aplicativos e onde as especificações o solicitem — pense em SOAP ou APIs web.

## Como fazer:
```C#
using System;
using System.Xml;
using System.Xml.Linq;

class Program
{
     static void Main()
     {
        var xmlString = @"<livraria>
                            <livro>
                              <titulo lang=""en"">Head First C#</titulo>
                              <preco>39.99</preco>
                            </livro>
                          </livraria>";

        // Analisa a string em um XDocument
        XDocument doc = XDocument.Parse(xmlString);

        // Adiciona um novo livro
        doc.Element("livraria").Add(
            new XElement("livro",
                new XElement("titulo", "Learning XML", new XAttribute("lang", "en")),
                new XElement("preco", 29.99)
            )
        );

        // Escreve o XML no console
        Console.WriteLine(doc);

        // Carrega o documento
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // Recupera todos os preços
        XmlNodeList precos = xmlDoc.GetElementsByTagName("preco");
        foreach (XmlNode preco in precos)
        {
            Console.WriteLine(preco.InnerText);
        }
     }
}

// Exemplo de Saída:
// <livraria>
//  <livro>
//    <titulo lang="en">Head First C#</titulo>
//    <preco>39.99</preco>
//  </livro>
//  <livro>
//    <titulo lang="en">Learning XML</titulo>
//    <preco>29.99</preco>
//  </livro>
// </livraria>
// 39.99
// 29.99
```

## Aprofundamento
O XML existe desde o final dos anos 90, o que o torna um avô em anos tecnológicos. Foi inventado para portabilidade de dados e facilidade de leitura humana. Alternativas como JSON estão agora pisando em seus calcanhares, especialmente em contextos web, porque é mais leve e, para muitos, mais simples de manusear. Mas o XML ainda mantém seu espaço em vários sistemas legados e certos protocolos de comunicação. Com XML, você obtém um esquema para validar sua estrutura e namespaces para evitar conflitos de tags — características que falam de sua maturidade pronta para a empresa.

Em C#, os namespaces `System.Xml.Linq` e `System.Xml` são duas grandes armas para trabalhar com XML. LINQ para XML (`XDocument`, `XElement`) é mais moderno e mais elegante — você viu sua mágica no exemplo. `XmlDocument` oferece a abordagem DOM (Modelo de Objeto de Documento) — um pouco antiga, mas algumas pessoas juram por seu poder.

## Veja também
- [MSDN – Visão Geral do LINQ para XML](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – Modelo de Objeto de Documento XML (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – Aprenda XML](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/xml.html)
