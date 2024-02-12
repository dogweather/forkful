---
title:                "HTML:n jäsennys"
aliases:
- /fi/java/parsing-html/
date:                  2024-02-03T19:12:29.359905-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML:n jäsennys"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTML:n jäsentäminen tarkoittaa merkintäkielen läpikäyntiä tietojen, kuten tekstin, linkkien tai muiden elementtien, poimimiseksi. Teemme sen vuorovaikuttaaksemme tai kaapiaksemme verkkosisältöä, automatisoidaksemme selailutehtäviä tai testataksemme web-sovelluksia.

## Kuinka:

Käytetään Jsoupia, kätevää kirjastoa työskentelyyn todellisen maailman HTML:n kanssa. Lisätään ensin riippuvuus:

```xml
<dependency>
    <groupId>org.jsoup</groupId>
    <artifactId>jsoup</artifactId>
    <version>1.15.2</version>
</dependency>
```

Nyt hauskaan osaan. Näin napataan verkkosivun otsikko ja tulostetaan se:

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class HtmlParser {
    public static void main(String[] args) throws IOException {
        String url = "http://example.com";
        Document doc = Jsoup.connect(url).get();
        String title = doc.title();
        System.out.println("Otsikko: " + title);
    }
}
```

Tuloste:

```
Otsikko: Example Domain
```

Entäpä linkkien poimiminen?

```java
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

// ... pää- tai muussa metodissa
Elements links = doc.select("a[href]");
for (Element link : links) {
    System.out.println("Linkki: " + link.attr("href"));
}
```

## Syväsukellus

Aikoinaan HTML:ää kesytettiin regex-kuvioilla, menetelmällä, joka oli virhealtis ja painajaismainen monimutkaisille dokumenteille. Myöhemmin 2000-luvulla tuli Jsoup, joka tarjosi jQueryn kaltaisen käyttöliittymän Javalle HTML:n jäsentämiseen, kulkemiseen ja manipuloimiseen.

Jsoup ei ole ainoa vaihtoehto. On HtmlUnit täysmittaiseen web-sovellusten testaukseen JavaScript-tuella, mutta se on raskaampi ja monimutkaisempi. Kevyempiin tehtäviin Apache Commons Validator on loistava vain URL-osoitteiden poimintaan.

Sisäisesti Jsoup käyttää DOM-jäsentäjää, joka mallintaa koko dokumentin muistissa puuna. Tämä lähestymistapa tekee HTML-rakenteen valitsemisesta ja navigoinnista helppoa. Lisäksi se on anteeksiantava huolimattoman HTML-koodin kanssa, korjaten ongelmat lennosta ja varmistaen luotettavan jäsentämisen.

Muista, kun teet kaavintaa, tarkistaa aina sivuston `robots.txt` ja käyttöehdot, jotta vältät lailliset ongelmat tai IP-eston.

## Katso Myös

- Jsoup Virallinen Dokumentaatio: https://jsoup.org/
- HtmlUnit: http://htmlunit.sourceforge.net/
- Apache Commons Validator: https://commons.apache.org/proper/commons-validator/
