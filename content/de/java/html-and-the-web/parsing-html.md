---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:20.109303-07:00
description: "Wie: Lassen Sie uns Jsoup verwenden, eine praktische Bibliothek f\xFC\
  r den Umgang mit HTML aus der realen Welt. F\xFCgen Sie zun\xE4chst die Abh\xE4\
  ngigkeit hinzu."
lastmod: '2024-03-13T22:44:53.761143-06:00'
model: gpt-4-0125-preview
summary: "Lassen Sie uns Jsoup verwenden, eine praktische Bibliothek f\xFCr den Umgang\
  \ mit HTML aus der realen Welt."
title: HTML parsen
weight: 43
---

## Wie:
Lassen Sie uns Jsoup verwenden, eine praktische Bibliothek für den Umgang mit HTML aus der realen Welt. Fügen Sie zunächst die Abhängigkeit hinzu:

```xml
<dependency>
    <groupId>org.jsoup</groupId>
    <artifactId>jsoup</artifactId>
    <version>1.15.2</version>
</dependency>
```

Nun zum spaßigen Teil. So greifen Sie auf den Titel einer Webseite zu und drucken ihn aus:

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class HtmlParser {
    public static void main(String[] args) throws IOException {
        String url = "http://example.com";
        Document doc = Jsoup.connect(url).get();
        String title = doc.title();
        System.out.println("Titel: " + title);
    }
}
```

Ausgabe:

```
Titel: Beispiel-Domain
```

Wie sieht es mit dem Extrahieren aller Links aus?

```java
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

// ... innerhalb der Hauptmethode oder einer anderen Methode
Elements links = doc.select("a[href]");
for (Element link : links) {
    System.out.println("Link: " + link.attr("href"));
}
```

## Tiefere Einblicke
Einst wurde HTML durch Regex-Muster gezähmt, eine Methode, die sowohl fehleranfällig als auch für komplexe Dokumente alptraumhaft war. Dann kam Jsoup in den späten Nullerjahren, das eine jQuery-ähnliche Schnittstelle für Java bot, um HTML zu parsen, zu durchqueren und zu manipulieren.

Jsoup ist nicht die einzige Wahl. Es gibt HtmlUnit für umfassende Tests von Webanwendungen mit JavaScript-Unterstützung, aber es ist schwerfälliger und komplexer. Für leichte Aufgaben ist der Apache Commons Validator großartig, nur für das Extrahieren von URLs.

Unter der Haube verwendet Jsoup einen DOM-Parser, der das gesamte Dokument im Speicher als Baum modelliert. Dieser Ansatz erleichtert das Auswählen und Navigieren der HTML-Struktur enorm. Außerdem ist er nachsichtig mit schlampigem HTML und behebt unterwegs Probleme, um eine robuste Analyse zu gewährleisten.

Denken Sie daran, beim Scrapen immer die `robots.txt` und die Nutzungsbedingungen einer Website zu überprüfen, um rechtliche Probleme oder ein IP-Verbot zu vermeiden.

## Siehe auch
- Jsoup Offizielle Dokumentation: https://jsoup.org/
- HtmlUnit: http://htmlunit.sourceforge.net/
- Apache Commons Validator: https://commons.apache.org/proper/commons-validator/
