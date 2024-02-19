---
aliases:
- /nl/kotlin/working-with-xml/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:24.169775-07:00
description: "Werken met XML omvat het parsen, cre\xEBren en manipuleren van XML-documenten\
  \ - een opmaaktaal voor gegevensopslag en -overdracht. Programmeurs doen dit\u2026"
lastmod: 2024-02-18 23:09:01.832510
model: gpt-4-0125-preview
summary: "Werken met XML omvat het parsen, cre\xEBren en manipuleren van XML-documenten\
  \ - een opmaaktaal voor gegevensopslag en -overdracht. Programmeurs doen dit\u2026"
title: Werken met XML
---

{{< edit_this_page >}}

## Wat & Waarom?
Werken met XML omvat het parsen, creëren en manipuleren van XML-documenten - een opmaaktaal voor gegevensopslag en -overdracht. Programmeurs doen dit omdat veel systemen nog steeds gegevens uitwisselen in XML-formaat, en dit is nodig voor legacy-ondersteuning en integratie met bestaande technologieën.

## Hoe te:
In Kotlin kun je de ingebouwde `javax.xml.parsers` gebruiken voor het parsen:

```Kotlin
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Document

fun parseXml(xmlData: String): Document {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder = dbFactory.newDocumentBuilder()
    return dBuilder.parse(xmlData.byteInputStream())
}
```
Om XML-documenten te creëren, kun je `javax.xml.transform` gebruiken:

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
Voorbeelduitvoer voor de conversie van een document naar String zou simpelweg je XML-inhoud in een tekenreeksformaat zijn.

## Diepe Duik
XML is sinds de jaren '90 een hoeksteen van web- en softwareontwikkeling, geliefd vanwege zijn leesbaarheid en gestructureerde hiërarchie. Hoewel JSON aan populariteit heeft gewonnen voor webservices vanwege zijn eenvoud en kleinere berichtgrootte, blijft XML dominant in zakelijke omgevingen, SOAP-gebaseerde webservices en configuraties (zoals Android-layoutbestanden).

Er zijn verschillende bibliotheken en API's naast de ingebouwde functies van Kotlin/Java voor XML-verwerking, zoals Simple XML Serialization en Jackson XML-module. Maar `javax.xml.parsers` en `javax.xml.transform` voldoen doorgaans aan de meeste behoeften zonder externe afhankelijkheden toe te voegen.

Bij het omgaan met XML in Kotlin omvatten belangrijke implementatiedetails het correct omgaan met tekenencodering en het beheren van XML-entiteiten om XML-injectieaanvallen te voorkomen. Wees je bewust van de complexiteit van namespaces en schema-validatie bij het parsen van XML om de integriteit van de gegevens te waarborgen.

## Zie Ook
- [Kotlin Documentatie](https://kotlinlang.org/docs/reference/)
- [Java DOM Documentatie](https://docs.oracle.com/javase/7/docs/api/org/w3c/dom/package-summary.html)
- [Simple XML Serialization](http://simple.sourceforge.net/)
- [Jackson XML Module](https://github.com/FasterXML/jackson-dataformat-xml)
