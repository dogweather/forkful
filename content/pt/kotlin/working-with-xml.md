---
title:                "Trabalhando com XML"
aliases:
- pt/kotlin/working-with-xml.md
date:                  2024-01-26T04:33:24.708320-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/working-with-xml.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Trabalhar com XML envolve analisar, criar e manipular documentos XML - uma linguagem de marcação para armazenamento e transferência de dados. Programadores fazem isso porque muitos sistemas ainda trocam dados no formato XML, e isso é necessário para suporte a legados e integração com tecnologias existentes.

## Como fazer:
Em Kotlin, você pode usar o `javax.xml.parsers` integrado para análise:

```Kotlin
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Document

fun parseXml(xmlData: String): Document {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder = dbFactory.newDocumentBuilder()
    return dBuilder.parse(xmlData.byteInputStream())
}
```
Para criar documentos XML, você pode usar `javax.xml.transform`:

```Kotlin
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import org.w3c.dom.Document
import java.io.StringWriter

fun convertDocumentToString(doc: Document): String {
    val transformer = TransformerFactory.newInstance().newTransformer()
    val result = StringWriter()
    transformer.transform(DOMSource(doc), StreamResult(result))
    return result.toString()
}
```
Um exemplo de saída para uma conversão de documento em String seria simplesmente o seu conteúdo XML em um formato de string.

## Aprofundando
XML tem sido uma pedra angular do desenvolvimento web e de software desde os anos 90, favorecido pela sua legibilidade e hierarquia estruturada. Embora o JSON tenha ganhado popularidade para serviços web devido à sua simplicidade e menor tamanho de mensagem, o XML permanece prevalente em ambientes empresariais, serviços web baseados em SOAP e configurações (como arquivos de layout Android).

Existem várias bibliotecas e APIs, além dos recursos integrados do Kotlin/Java para manuseio de XML, como Simple XML Serialization e Jackson XML module. Mas `javax.xml.parsers` e `javax.xml.transform` normalmente atendem a maioria das necessidades sem adicionar dependências externas.

Ao lidar com XML em Kotlin, os detalhes de implementação chave incluem o tratamento adequado da codificação de caracteres e o gerenciamento de entidades XML para prevenir ataques de injeção de XML. Tenha atenção às complexidades de namespace e validação de esquema ao analisar o XML para garantir a integridade dos dados.

## Veja Também
- [Documentação do Kotlin](https://kotlinlang.org/docs/reference/)
- [Documentação do DOM do Java](https://docs.oracle.com/javase/7/docs/api/org/w3c/dom/package-summary.html)
- [Simple XML Serialization](http://simple.sourceforge.net/)
- [Módulo XML do Jackson](https://github.com/FasterXML/jackson-dataformat-xml)
