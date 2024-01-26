---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:32:20.554731-07:00
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
HTML:n jäsentäminen tarkoittaa HTML-koodin lukemista ja siitä halutun tiedon erottamista. Ohjelmoijat tekevät tätä muun muassa web-sivujen datan kaavintaan ja sisällön automaattiseen käsittelyyn.

## How to: (Kuinka tehdään:)
Java tarjoaa useita kirjastoja HTML:n käsittelyyn. Esimerkkinä käytetään JSoup-kirjastoa, joka on helppokäyttöinen ja tarjoaa voimakkaita HTML-analysointiominaisuuksia.

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class HtmlParserExample {
    public static void main(String[] args) {
        String html = "<html><head><title>Esimerkki sivu</title></head>"
                    + "<body><p>Parasta Java-tietoa.</p></body></html>";
        
        Document doc = Jsoup.parse(html);
        String title = doc.title();
        System.out.println("Otsikko: " + title);
        
        Elements paragraphs = doc.select("p");
        for(Element paragraph : paragraphs) {
            System.out.println("Kappale: " + paragraph.text());
        }
    }
}
```

Konsolin tulostus:
```
Otsikko: Esimerkki sivu
Kappale: Parasta Java-tietoa.
```

## Deep Dive (Sukellus syvyyksiin):
HTML:n jäsentäminen ei ole uusi käsite. Webin alkuaikoina oli vain vähän työkaluja sen käsittelyyn. Nyt on monia kirjastoja, kuten JSoup, HtmlUnit, ja JsoupXpath. JSoup on suosittu Java-kirjasto, koska se on nopea, voimakas ja helppokäyttöinen. Sen avulla voit muokata HTML-elementtejä, valita dataa CSS-selektoreiden kanssa ja käsitellä muotoiluja.

## See Also (Katso myös):
- [JSoup Official Documentation](https://jsoup.org/)
- [W3C HTML Parsing Guidelines](https://www.w3.org/TR/html5/syntax.html#parsing)
- [HtmlUnit](http://htmlunit.sourceforge.net/)
