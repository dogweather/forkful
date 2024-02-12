---
title:                "Å jobbe med XML"
aliases:
- /no/kotlin/working-with-xml.md
date:                  2024-01-26T04:33:12.096022-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/working-with-xml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med XML innebærer parsing, oppretting og manipulering av XML-dokumenter – et markup-språk for datalagring og -overføring. Programmerere gjør dette fordi mange systemer fortsatt utveksler data i XML-format, og det er nødvendig for å opprettholde støtten til eldre systemer og for integrering med eksisterende teknologier.

## Hvordan:
I Kotlin kan du bruke den innebygde `javax.xml.parsers` for parsing:

```Kotlin
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Document

fun parseXml(xmlData: String): Document {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder = dbFactory.newDocumentBuilder()
    return dBuilder.parse(xmlData.byteInputStream())
}
```
For å opprette XML-dokumenter, kan du bruke `javax.xml.transform`:

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
Eksempel på utdata for en dokumentkonvertering til streng ville rett og slett være ditt XML-innhold i strengformat.

## Dypdykk
XML har vært en hjørnestein i web- og programvareutvikling siden 90-tallet, verdsatt for sin lesbarhet og strukturerte hierarki. Selv om JSON har blitt populært for webtjenester på grunn av sin enkelhet og mindre meldingsstørrelse, er XML fortsatt utbredt i bedriftsmiljøer, SOAP-baserte webtjenester og konfigurasjoner (som Android layout-filer).

Det finnes ulike biblioteker og APIer i tillegg til de innebygde funksjonene til Kotlin/Java for XML-håndtering, som Simple XML Serialization og Jackson XML-modulen. Men `javax.xml.parsers` og `javax.xml.transform` dekker vanligvis de fleste behov uten å legge til eksterne avhengigheter.

Når du arbeider med XML i Kotlin, inkluderer viktige implementeringsdetaljer å håndtere tegnkoding på riktig måte og å administrere XML-entiteter for å forhindre XML-injeksjonsangrep. Vær oppmerksom på kompleksitetene rundt navneområder og skjemavalidering når du parser XML for å sikre dataintegritet.

## Se Også
- [Kotlin Dokumentasjon](https://kotlinlang.org/docs/reference/)
- [Java DOM Dokumentasjon](https://docs.oracle.com/javase/7/docs/api/org/w3c/dom/package-summary.html)
- [Simple XML Serialization](http://simple.sourceforge.net/)
- [Jackson XML-modul](https://github.com/FasterXML/jackson-dataformat-xml)
