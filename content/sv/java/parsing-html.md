---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka (parse) HTML handlar om att konvertera HTML-data till ett mer "hanterbart" format, såsom ett trädstruktur. Det är viktigt för programmerare att gör detta för att kunna bearbeta, analysera, och manipulera webbinnehåll på ett effektivt sätt.

## Hur man gör:
I Java kan du använda biblioteket JSoup för att göra detta. JSoup är en robust och flexibel bibliotek för att arbeta med HTML-data. Till exempel:

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class Main {
    public static void main(String[] args) throws Exception {
        String html = "<html><head><title>Test</title></head>"
                    + "<body><p>Testparagraf.</p></body></html>";
        Document doc = Jsoup.parse(html);    
        System.out.println(doc.title());
        System.out.println(doc.body().text());
    }
}
```
Kör du den här koden, så skulle output bli:
```
Test
Testparagraf.
```
## Djupdykning
För historisk kontext, HTML-tolkning, eller parsing, har funnits sedan vi började använda webben. Men med Java och bibliotek som Jsoup, har det aldrig varit enklare. 

Det finns andra alternativ förutom Jsoup, till exempel HtmlCleaner och jHtml, men många anser att Jsoup är mer intuitivt och kraftfullt tack vare dess stöd för CSS-selektor syntax.

För implementation, Html-tolkning fungerar genom att programmet går igenom HTML-koden från början till slut, tolkar varje tagg och skapar överensstämmande noder i det resulterande dokumentträdet.

## Se också
Här är några relaterade källor för mer detaljerade studier:
- JSoup API dokumentation: https://jsoup.org/apidocs/
- Officiell Java dokumentation: https://docs.oracle.com/javase/tutorial/
- ‘HtmlCleaner’ biblioteket: http://htmlcleaner.sourceforge.net/
- ‘jHtml’ biblioteket: https://jhy.io/jHtml