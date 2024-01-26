---
title:                "XML:n käsittely"
date:                  2024-01-26T04:33:00.303210-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML:n käsittely"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/working-with-xml.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
XML:n käsittelyyn kuuluu XML-dokumenttien jäsentäminen, luominen ja manipulointi – merkintäkieli datan tallennukseen ja siirtoon. Ohjelmoijat tekevät tätä, koska monet järjestelmät vaihtavat edelleen tietoja XML-muodossa, ja sitä tarvitaan vanhojen järjestelmien tukemiseen ja olemassa olevien teknologioiden kanssa integroimiseen.

## Kuinka:
Kotlin-kielessä voit käyttää sisäänrakennettua `javax.xml.parsers` -kirjastoa jäsentämiseen:

```Kotlin
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Document

fun parseXml(xmlData: String): Document {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder = dbFactory.newDocumentBuilder()
    return dBuilder.parse(xmlData.byteInputStream())
}
```
XML-dokumenttien luomiseen saatat käyttää `javax.xml.transform`:

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
Yksinkertainen tulos dokumentin muuttamisesta merkkijonoksi olisi yksinkertaisesti XML-sisältösi merkkijonomuodossa.

## Syväsukellus
XML on ollut web- ja ohjelmistokehityksen kulmakivi 90-luvulta lähtien, suosittu luettavuutensa ja jäsennellyn hierarkiansa ansiosta. Vaikka JSON on saavuttanut suosiota verkkopalveluissa sen yksinkertaisuuden ja pienemmän viestikoon vuoksi, XML on edelleen vallalla yritysympäristöissä, SOAP-pohjaisissa verkkopalveluissa ja konfiguraatioissa (kuten Androidin asettelutiedostoissa).

Kotlin/Java:n sisäänrakennettujen ominaisuuksien lisäksi XML-käsittelyyn on erilaisia kirjastoja ja API-rajapintoja, kuten Simple XML Serialization ja Jackson XML -moduuli. Mutta `javax.xml.parsers` ja `javax.xml.transform` tavallisesti täyttävät useimmat tarpeet lisäämättä ulkopuolisia riippuvuuksia.

Käsitellessäsi XML:ää Kotlinissa, keskeisiä toteutuksen yksityiskohtia ovat merkistökoodauksen oikea käsittely ja XML-entiteettien hallinta XML-injektiohyökkäysten estämiseksi. Ole tietoinen nimiavaruuden monimutkaisuuksista ja skeemavalidoinnista XML:n jäsentämisessä varmistaaksesi datan eheyden.

## Katso Myös
- [Kotlin Dokumentaatio](https://kotlinlang.org/docs/reference/)
- [Java DOM Dokumentaatio](https://docs.oracle.com/javase/7/docs/api/org/w3c/dom/package-summary.html)
- [Simple XML Serialization](http://simple.sourceforge.net/)
- [Jackson XML Module](https://github.com/FasterXML/jackson-dataformat-xml)