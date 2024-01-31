---
title:                "Tolka HTML"
date:                  2024-01-20T15:32:25.132505-07:00
html_title:           "Arduino: Tolka HTML"
simple_title:         "Tolka HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Parsing av HTML är att läsa och tolka HTML-koden så att dess struktur och innehåll blir begripligt och hanterbart för program. Programmerare gör det för att automatisera webbskrapning, kontrollera innehåll eller interagera med webbsidor i applikationer.

## Hur gör man?:
För att parsa HTML i Java kan vi använda Jsoup, ett kraftfullt bibliotek för att hantera HTML. Här är ett snabbt exempel:

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class HtmlParserExample {
    public static void main(String[] args) {
        String html = "<html><head><title>Exempelsida</title></head>"
                + "<body><p>Dett är en <a href='http://example.com/'>länk</a>.</p></body></html>";
        
        Document doc = Jsoup.parse(html);
        Element link = doc.select("a").first();
        
        System.out.println("Länktext: " + link.text());
        System.out.println("URL: " + link.attr("href"));
    }
}
```
Körning av koden ger följande output:
```
Länktext: länk
URL: http://example.com/
```

## Djupdykning:
Parsing av HTML har funnits så länge som HTML själv. Det började med enkla verktyg i CGI och Perl och har utvecklats till komplexa bibliotek som Jsoup i Java, Beautiful Soup i Python och Cheerio i Node.js. Medan Jsoup är lätt att använda och ger kraftfulla selektorer liknande jQuery, finns det också andra Java-bibliotek som HtmlUnit som är mer till för att simulera webbläsare. Implementationsdetaljer är viktiga; att välja rätt bibliotek kan spara tid och undvika fallgropar som att hantera felaktig eller ofullständig HTML som ofta förekommer i verkliga webbsidor.

## Se även:
- Jsoup officiella webbplats: https://jsoup.org/
- HtmlUnit officiella webbplats: https://htmlunit.sourceforge.io/
- W3C HTML parser jämförelse: https://www.w3.org/html/wg/drafts/html/master/single-page.html#parsing-html-documents
