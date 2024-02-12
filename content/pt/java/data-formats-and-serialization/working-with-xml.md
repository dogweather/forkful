---
title:                "Trabalhando com XML"
aliases: - /pt/java/working-with-xml.md
date:                  2024-01-26T04:32:36.947606-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/working-with-xml.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?
Trabalhar com XML envolve analisar, consultar e manipular documentos XML com Java. Os programadores fazem isso para troca de dados, gerenciamento de configuração e porque muitos sistemas legados e APIs se comunicam usando XML.

## Como fazer:
Java oferece APIs como DOM (Modelo de Objeto de Documento), SAX (API Simples para XML) e StAX (API de Streaming para XML) para trabalhar com XML. Aqui está um exemplo de DOM para analisar um arquivo XML:

```java
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class XmlParser {
    public static void main(String[] args) {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document doc = builder.parse("data.xml");
            
            doc.getDocumentElement().normalize();
            NodeList nodeList = doc.getElementsByTagName("employee");
            
            for (int i = 0; i < nodeList.getLength(); i++) {
                Element element = (Element) nodeList.item(i);
                System.out.println("Nome: " + element.getElementsByTagName("name").item(0).getTextContent());
                System.out.println("Idade: " + element.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Suponha que `data.xml` pareça com isto:

```xml
<employees>
    <employee>
        <name>Jane Doe</name>
        <age>30</age>
    </employee>
    <employee>
        <name>John Doe</name>
        <age>40</age>
    </employee>
</employees>
```

A saída seria:

```
Nome: Jane Doe
Idade: 30
Nome: John Doe
Idade: 40
```

## Aprofundando
O XML existe desde o final dos anos 90, fornecendo uma maneira estruturada e flexível de trocar dados entre diferentes sistemas. Embora o JSON tenha se tornado mais popular para novas APIs da web devido à sua sintaxe mais simples e integração apertada com JavaScript, o XML continua sendo amplamente utilizado em ambientes empresariais, serviços web baseados em SOAP e padrões de documentos como o Office Open XML para Microsoft Office.

Quando se trata de analisar XML em Java, a API DOM é excelente para documentos menores: é baseada em árvore e permite acesso total à estrutura XML na memória. No entanto, para arquivos maiores, pode ser intensivo em memória. SAX e StAX são mais amigáveis à memória, pois são movidos a eventos e baseados em fluxo, respectivamente, mas podem ser menos convenientes para navegar pelas estruturas XML.

Para criar ou modificar XML, o Java também oferece os pacotes javax.xml.transform e javax.xml.bind (JAXB). JAXB fez parte do Java SE até a versão 10, depois, tornou-se uma biblioteca separada devido à remoção dos módulos Java EE do Java SE. É uma maneira orientada por anotações de serializar objetos Java para XML e vice-versa.

## Veja Também
Confira estas fontes relacionadas para mais informações sobre trabalhar com XML em Java:
- [API Java para Processamento de XML (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [Arquitetura Java para Ligação com XML (JAXB)](https://javaee.github.io/jaxb-v2/)
- [Guia da Oracle para XML em Java](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [Tecnologia W3C XML](https://www.w3.org/standards/xml/)
- [Stack Overflow: Questões etiquetadas como 'java' e 'xml'](https://stackoverflow.com/questions/tagged/java+xml)
