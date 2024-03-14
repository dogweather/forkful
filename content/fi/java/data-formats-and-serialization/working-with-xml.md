---
date: 2024-01-26 04:32:35.888627-07:00
description: "XML:n kanssa ty\xF6skentely k\xE4sitt\xE4\xE4 XML-dokumenttien j\xE4\
  sent\xE4misen, kyselyiden tekemisen ja manipuloinnin Java-kielell\xE4. Ohjelmoijat\
  \ tekev\xE4t t\xE4t\xE4\u2026"
lastmod: '2024-03-13T22:44:56.470175-06:00'
model: gpt-4-0125-preview
summary: "XML:n kanssa ty\xF6skentely k\xE4sitt\xE4\xE4 XML-dokumenttien j\xE4sent\xE4\
  misen, kyselyiden tekemisen ja manipuloinnin Java-kielell\xE4. Ohjelmoijat tekev\xE4\
  t t\xE4t\xE4\u2026"
title: "XML:n k\xE4sittely"
---

{{< edit_this_page >}}

## Mikä ja miksi?
XML:n kanssa työskentely käsittää XML-dokumenttien jäsentämisen, kyselyiden tekemisen ja manipuloinnin Java-kielellä. Ohjelmoijat tekevät tätä tiedonvaihdon, konfiguraationhallinnan sekä siksi, että monet vanhat järjestelmät ja rajapinnat kommunikoivat XML:n avulla.

## Kuinka:
Java tarjoaa API:ja kuten DOM (Document Object Model), SAX (Simple API for XML) ja StAX (Streaming API for XML) XML:n käsittelyyn. Tässä on DOM-esimerkki XML-tiedoston jäsentämiseksi:

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
                System.out.println("Nimi: " + element.getElementsByTagName("name").item(0).getTextContent());
                System.out.println("Ikä: " + element.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Oletetaan, että `data.xml` näyttää tältä:

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

Tuloste olisi:

```
Nimi: Jane Doe
Ikä: 30
Nimi: John Doe
Ikä: 40
```

## Syväsukellus
XML on ollut olemassa 90-luvun lopulta lähtien, tarjoten strukturoitua ja joustavaa tapaa vaihtaa dataa eri järjestelmien välillä. Vaikka JSON on tullut suositummaksi uusien web-rajapintojen kanssa sen yksinkertaisemman syntaksin ja tiiviin integraation JavaScriptin kanssa vuoksi, XML on silti laajasti käytössä yritysympäristöissä, SOAP-pohjaisissa web-palveluissa ja dokumenttistandardeissa kuten Office Open XML Microsoft Officelle.

Kun kyse on XML:n jäsentämisestä Javassa, DOM API on mainio pienempien dokumenttien kanssa: se on puupohjainen ja antaa täyden pääsyn XML-rakenteeseen muistissa. Kuitenkin suurempien tiedostojen kohdalla se voi olla muistintehokas. SAX ja StAX ovat muistin kannalta ystävällisempiä, koska ne ovat tapahtumaohjattuja ja suoratoistoon perustuvia vastaavasti, mutta ne voivat olla vähemmän käteviä XML-rakenteiden navigoinnissa.

XML:n luomiseen tai muokkaamiseen Java tarjoaa myös javax.xml.transform ja javax.xml.bind (JAXB) -paketit. JAXB oli osa Java SE:ta versioon 10 asti, sen jälkeen siitä tuli erillinen kirjasto Java EE -moduulien poiston myötä Java SE:stä. Se on annotaatioon perustuva tapa serialisoida Java-objektit XML:ksi ja päinvastoin.

## Katso myös
Tutustu näihin liittyviin lähteisiin saadaksesi lisätietoa XML:n kanssa työskentelystä Javassa:
- [Java API for XML Processing (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [Java Architecture for XML Binding (JAXB)](https://javaee.github.io/jaxb-v2/)
- [Oraclen opas XML:ään Javassa](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [W3C XML Technology](https://www.w3.org/standards/xml/)
- [Stack Overflow: Kysymykset merkitty 'java' ja 'xml'](https://stackoverflow.com/questions/tagged/java+xml)
