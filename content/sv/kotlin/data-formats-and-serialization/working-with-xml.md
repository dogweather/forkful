---
date: 2024-01-26 04:33:20.516165-07:00
description: "Att arbeta med XML inneb\xE4r att tolka, skapa och manipulera XML-dokument\
  \ \u2013 ett m\xE4rkspr\xE5k f\xF6r lagring och \xF6verf\xF6ring av data. Programmerare\
  \ g\xF6r detta\u2026"
lastmod: 2024-02-19 22:04:57.111277
model: gpt-4-0125-preview
summary: "Att arbeta med XML inneb\xE4r att tolka, skapa och manipulera XML-dokument\
  \ \u2013 ett m\xE4rkspr\xE5k f\xF6r lagring och \xF6verf\xF6ring av data. Programmerare\
  \ g\xF6r detta\u2026"
title: Att arbeta med XML
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med XML innebär att tolka, skapa och manipulera XML-dokument – ett märkspråk för lagring och överföring av data. Programmerare gör detta eftersom många system fortfarande utbyter data i XML-format, och det behövs för stöd av äldre system och integration med befintliga teknologier.

## Hur man gör:
I Kotlin kan du använda den inbyggda `javax.xml.parsers` för tolkning:

```Kotlin
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Document

fun parseXml(xmlData: String): Document {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder = dbFactory.newDocumentBuilder()
    return dBuilder.parse(xmlData.byteInputStream())
}
```
För att skapa XML-dokument kan du använda `javax.xml.transform`:

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
Ett exempel på utdata för konvertering av ett dokument till sträng skulle helt enkelt vara ditt XML-innehåll i strängformat.

## Djupdykning
XML har varit en grundsten inom webb- och mjukvaruutveckling sedan 90-talet, uppskattad för sin läsbarhet och strukturerade hierarki. Även om JSON har blivit populärt för webbtjänster på grund av sin enkelhet och mindre meddelandestorlek, är XML fortfarande utbrett i företagsmiljöer, SOAP-baserade webbtjänster och konfigurationer (som Android-layoutfiler).

Det finns olika bibliotek och API'er förutom de inbyggda funktionerna i Kotlin/Java för hantering av XML, såsom Simple XML Serialization och Jackson XML-modulen. Men `javax.xml.parsers` och `javax.xml.transform` brukar oftast täcka de flesta behov utan att lägga till externa beroenden.

När du hanterar XML i Kotlin inkluderar viktiga genomförandedetaljer att hantera teckenkodning korrekt och hantera XML-entiteter för att förhindra XML-injektionsattacker. Var medveten om komplexiteten i namnrymden och schemavalidering vid tolkning av XML för att säkerställa dataintegritet.

## Se också
- [Kotlin-dokumentation](https://kotlinlang.org/docs/reference/)
- [Java DOM-dokumentation](https://docs.oracle.com/javase/7/docs/api/org/w3c/dom/package-summary.html)
- [Simple XML Serialization](http://simple.sourceforge.net/)
- [Jackson XML-modul](https://github.com/FasterXML/jackson-dataformat-xml)
