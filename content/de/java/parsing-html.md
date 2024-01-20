---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/parsing-html.md"
---

{{< edit_this_page >}}

# HTML parsen: Was & Warum?
HTML-Parsing ist der Prozess, bei dem eine HTML-Datei in ihre Elemente zerlegt wird. Dies wird oft in Programmen durchgeführt, die Informationen von Webseiten extrahieren oder manipulieren müssen.

## So geht's: 
Hier ist ein einfaches Beispiel, wie man HTML in Java mit der Bibliothek Jsoup parst. Diese Bibliothek steht unter einer freien Lizenz und kann einfach per Maven oder Gradle installiert werden. 

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class Main {

    public static void main(String[] args) throws Exception {
        // HTML-Code als String
        String html = "<html><head><title>Erste Parse</title></head>"
                + "<body><p>Parsed HTML in einen Körper.</p></body></html>";
        // Dokument erstellen
        Document doc = Jsoup.parse(html);

        // Titel des Dokuments auslesen
        String title = doc.title();
        System.out.println("Title : " + title);
        
        // Elemente per CSS-Selector auswählen
        Elements paragraphs = doc.select("p");
        for (Element paragraph : paragraphs) {
            System.out.println(paragraph.text());
        }
    }
}
```
Sample Output:
```
Title : Erste Parse
Parsed HTML in einen Körper.
```

## Vertiefung
Anfang der 90er Jahre, als das Web noch jung war, gab es das HTML-Parsing noch nicht. Andrew H. W. Tanenbaum sagte einmal "Das schöne am HTML ist, dass es fehlertolerant ist." Mitte der 90er Jahre, zusammen mit dem Boom des Internets, erschienen die ersten Bibliotheken, um HTML zu parsen. XmlPull und HTML Tidy sind Beispiele für frühe Bibliotheken.

Heute haben wir mehrere Alternativen zum Parsen von HTML in Java, darunter Jsoup, HTMLUnit, TagSoup und JDom.

Beim Parsen lesen und analysieren wir den HTML-Code Zeile für Zeile. Welche Methode für Sie am besten geeignet ist, hängt von Ihren speziellen Anforderungen ab. Jede Methode hat ihre eigenen Vorteile und Einschränkungen.

## Siehe auch
1. [Jsoup offizielle Dokumentation](https://jsoup.org/)
2. [HTMLUnit offizielle Dokumentation](http://htmlunit.sourceforge.net/)