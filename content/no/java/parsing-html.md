---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Parsing av HTML er en prosess hvor vi trekker ut data fra en HTML fil. Vi gjør dette for å hente bestemte elementer og verdier fra filen for videre bruk i våre programmer.

## Hvordan?

Her er et grunnleggende eksempel på HTML-parsing i Java ved hjelp av Jsoup. Vi vil hente alle lenkene fra en web-side.

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class HtmlParser {

    public static void main(String[] args) throws IOException {
        Document doc = Jsoup.connect("http://www.example.com").get();
        Elements links = doc.select("a[href]");
        for (Element link : links) {
            System.out.println("\nlink: " + link.attr("href"));
            System.out.println("tekst: " + link.text());
        }
    }
}
```

Utgangen fra dette blir en liste over lenker og tilhørende tekster på siden.

## Dypdykk

Parsing av HTML startet først da webutvikling begynte, og programmerere trengte å utvinne data fra web-sider. I disse dager har vi biblioteker som Jsoup, HtmlUnit, og Jtidy som kan gjøre jobben for oss.

Når det gjelder alternativer, du kan faktisk parse xml på en lignende måte som HTML siden HTML er en form for XML. Java har innebygd støtte for parsing av XML filer ved hjelp av DocumentBuilderFactory.

Når det gjelder implementasjon av disse bibliotekene, Jsoup, for eksempel, bruker DOM parsing for å utvinne data. Det lager en "tree" av objekter fra HTML-filen, og disse kan manipuleres etter behov.

## Se Også

For mer info om jsoup og hvordan du bruker det, sjekk [Jsoup Official Doc](https://jsoup.org/)

For å arbeide med XML parsing, [Java DOM Parser](https://docs.oracle.com/javase/tutorial/jaxp/dom/readingXML.html) er en bra sted å begynne.

For alternativer til Jsoup, sjekk både [HTML Unit](http://htmlunit.sourceforge.net/) og [Jtidy](http://jtidy.sourceforge.net/).