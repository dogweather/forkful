---
title:                "Praca z XML"
date:                  2024-01-26T04:33:18.961514-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/working-with-xml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z XML obejmuje parsowanie, tworzenie i manipulowanie dokumentami XML - językiem znaczników do przechowywania i przesyłania danych. Programiści robią to, ponieważ wiele systemów nadal wymienia dane w formacie XML, co jest potrzebne do wsparcia dziedziczenia i integracji z istniejącymi technologiami.

## Jak to zrobić:
W Kotlinie możesz użyć wbudowanego `javax.xml.parsers` do parsowania:

```Kotlin
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Document

fun parseXml(xmlData: String): Document {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder = dbFactory.newDocumentBuilder()
    return dBuilder.parse(xmlData.byteInputStream())
}
```
Aby tworzyć dokumenty XML, możesz użyć `javax.xml.transform`:

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
Przykładowy wynik konwersji dokumentu na ciąg znaków to po prostu zawartość XML w formacie ciągu.

## Pogłębiona analiza
XML od lat 90. stanowi kamień węgielny rozwoju sieci i oprogramowania, ceniony za swoją czytelność i strukturalną hierarchię. Chociaż JSON zyskał popularność w usługach sieciowych ze względu na swoją prostotę i mniejszy rozmiar wiadomości, XML pozostaje powszechny w środowiskach korporacyjnych, usługach sieciowych opartych na SOAP i konfiguracjach (takich jak pliki układu Android).

Istnieją różne biblioteki i API oprócz wbudowanych funkcji Kotlin/Java do obsługi XML, takie jak Simple XML Serialization i moduł XML Jacksona. Ale `javax.xml.parsers` i `javax.xml.transform` zazwyczaj spełniają większość potrzeb bez dodawania zewnętrznych zależności.

Podczas pracy z XML w Kotlinie kluczowe szczegóły implementacji obejmują odpowiednie obsługiwanie kodowania znaków i zarządzanie encjami XML, aby zapobiec atakom iniekcji XML. Bądź świadomy złożoności przestrzeni nazw i walidacji schematu podczas parsowania XML, aby zapewnić integralność danych.

## Zobacz również
- [Dokumentacja Kotlin](https://kotlinlang.org/docs/reference/)
- [Dokumentacja Java DOM](https://docs.oracle.com/javase/7/docs/api/org/w3c/dom/package-summary.html)
- [Simple XML Serialization](http://simple.sourceforge.net/)
- [Moduł XML Jacksona](https://github.com/FasterXML/jackson-dataformat-xml)
