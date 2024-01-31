---
title:                "HTML parsen"
date:                  2024-01-20T15:32:30.380110-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"

category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
HTML-Parsing ist das Auslesen und Umformen von HTML-Dokumenten in eine für Programme bearbeitbare Struktur. Wir machen das, um Inhalte von Webseiten zu extrahieren, automatisiert zu verarbeiten oder zu scrapen.

## How to: (Wie geht das?)
Hier ist ein Beispiel mit der JSoup-Library, die in Java für HTML-Parsing verwendet wird. Installiere sie zuerst mit Maven oder Gradle.

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class HtmlParserBeispiel {

    public static void main(String[] args) {
        final String html = "<html><head><title>Beispielseite</title></head>"
                + "<body><p>Dies ist ein Absatz.</p><a href='https://beispiel.com'>Link</a></body></html>";

        Document doc = Jsoup.parse(html);
        
        String title = doc.title();
        System.out.println("Titel der Seite: " + title);
        
        Elements paragraphs = doc.select("p");
        for (Element paragraph : paragraphs) {
            System.out.println("Absatz: " + paragraph.text());
        }
        
        Element link = doc.select("a").first();
        System.out.println("Link: " + link.attr("href"));
    }
}
```

Und das ist die Ausgabe:

```
Titel der Seite: Beispielseite
Absatz: Dies ist ein Absatz.
Link: https://beispiel.com
```

## Deep Dive (Tiefergehende Infos)
Das Parsen von HTML ist so alt wie das Web selbst. Im Laufe der Zeit sind viele Libraries entstanden, wie JSoup, HTMLCleaner und TagSoup. JSoup ist beliebt wegen seiner jQuery-ähnlichen Syntax und Fähigkeit, auch fehlerhaftes HTML zu verstehen. Beim Parsen ist es wichtig, auf die korrekte Struktur zu achten und möglichst ressourcenschonend vorzugehen, um die Server nicht zu überlasten.

## See Also (Siehe auch)
- JSoup Dokumentation: https://jsoup.org/
- W3C HTML5 Spezifikation: https://www.w3.org/TR/html5/
- HTMLCleaner Projektseite: http://htmlcleaner.sourceforge.net/
- TagSoup GitHub Repository: https://github.com/ccil/cowan/tagsoup

Beachte: Dieser Artikel reflektiert den Stand von Java bis einschließlich März 2023. Informiere dich über mögliche Aktualisierungen bei den verwendeten Bibliotheken.
