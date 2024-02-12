---
title:                "Arbeiten mit XML"
aliases:
- de/kotlin/working-with-xml.md
date:                  2024-01-26T04:32:43.294909-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/working-with-xml.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Arbeit mit XML umfasst das Parsen, Erstellen und Manipulieren von XML-Dokumenten – einer Auszeichnungssprache für die Datenspeicherung und -übertragung. Programmierer tun dies, weil viele Systeme Daten immer noch im XML-Format austauschen, und es wird für die Legacy-Unterstützung und die Integration mit bestehenden Technologien benötigt.

## Wie:
In Kotlin können Sie die integrierten `javax.xml.parsers` zum Parsen verwenden:

```Kotlin
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Document

fun parseXml(xmlData: String): Document {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder = dbFactory.newDocumentBuilder()
    return dBuilder.parse(xmlData.byteInputStream())
}
```
Um XML-Dokumente zu erstellen, könnten Sie `javax.xml.transform` verwenden:

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
Ein Beispiel für die Ausgabe einer Dokumentenumwandlung in einen String wäre einfach Ihr XML-Inhalt in einem String-Format.

## Tiefergehend
XML ist seit den 90er Jahren ein Eckpfeiler der Web- und Softwareentwicklung, bevorzugt wegen seiner Lesbarkeit und strukturierten Hierarchie. Obwohl JSON aufgrund seiner Einfachheit und kleineren Nachrichtengröße für Webdienste an Beliebtheit gewonnen hat, bleibt XML in Unternehmensumgebungen, SOAP-basierten Webdiensten und Konfigurationen (wie Android-Layout-Dateien) weit verbreitet.

Es gibt verschiedene Bibliotheken und APIs neben den in Kotlin/Java integrierten Funktionen für die XML-Verarbeitung, wie Simple XML Serialization und Jackson XML-Modul. Aber `javax.xml.parsers` und `javax.xml.transform` decken typischerweise die meisten Bedürfnisse ab, ohne externe Abhängigkeiten hinzuzufügen.

Bei der Arbeit mit XML in Kotlin umfassen wichtige Implementierungsdetails den ordnungsgemäßen Umgang mit Zeichenkodierung und das Verwalten von XML-Entitäten, um XML-Injection-Angriffe zu verhindern. Achten Sie auf die Komplexitäten von Namensräumen und die Schemaüberprüfung beim Parsen von XML, um die Datenintegrität zu gewährleisten.

## Siehe auch
- [Kotlin-Dokumentation](https://kotlinlang.org/docs/reference/)
- [Java DOM-Dokumentation](https://docs.oracle.com/javase/7/docs/api/org/w3c/dom/package-summary.html)
- [Simple XML Serialization](http://simple.sourceforge.net/)
- [Jackson XML-Modul](https://github.com/FasterXML/jackson-dataformat-xml)
