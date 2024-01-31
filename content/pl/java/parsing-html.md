---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:32:18.276505-07:00
html_title:           "Bash: Przetwarzanie HTML"
simple_title:         "Przetwarzanie HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Parsing HTML to proces wydobywania danych ze struktur HTML. Programiści to robią, by manipulować treścią, wyciągać konkretne informacje, lub zmieniać struktury stron internetowych.

## Jak to zrobić?
Do parsowania HTML w Javie można użyć biblioteki jsoup. Oto prosty przykład:

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.select.Elements;

public class HtmlParser {
    public static void main(String[] args) {
        String html = "<html><head><title>Pierwsza lekcja HTML</title></head>"
                + "<body><p>Witaj na naszej stronie!</p></body></html>";

        Document doc = Jsoup.parse(html);
        Elements paragraphs = doc.select("p");
        
        for (int i = 0; i < paragraphs.size(); i++) {
            System.out.println("Akapit " + (i + 1) + ": " + paragraphs.get(i).text());
        }
    }
}
```

Sample output:
```
Akapit 1: Witaj na naszej stronie!
```

## W pogłębieniu
Parsowanie HTML istnieje odkąd HTML stał się standardem w internetowych dokumentach. W przeszłości używano do tego różnorodne narzędzia, włączając w to wyrażenia regularne, które były słabo dostosowane do złożoności HTML.

Alternatywą do jsoup mogą być inne biblioteki jak HtmlUnit lub tag soup. Warto jednak pamiętać, że jsoup jest wysoce ceniony za swoją łatwość użycia i efektywność.

Implementując parser HTML, musimy rozważyć wydajność (czas odpowiedzi i zużycie pamięci), obsługę nietypowych lub niepoprawnych struktur HTML oraz potencjalne zagrożenia związane z bezpieczeństwem, jak ataki XSS przez nieoczyszczone dane.

## Zobacz także
- [jsoup: Java HTML Parser](https://jsoup.org/)
- [Tutorialspoint - Java XML Parser](https://www.tutorialspoint.com/java_xml/java_dom_parse_document.htm)
- [W3Schools - HTML DOM](https://www.w3schools.com/js/js_htmldom.asp)
