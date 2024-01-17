---
title:                "Eine Webseite herunterladen"
html_title:           "Java: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Download einer Webseite wird der Inhalt einer Website von einem Server auf Ihren Computer übertragen. Programmierer nutzen diese Funktion, um Inhalte von Webseiten in ihre Anwendungen zu integrieren oder um Daten für Analysen zu sammeln.

## Wie geht's:
Um eine Webseite in Java herunterzuladen, können Sie die Java-Standardbibliothek verwenden oder eine externe Bibliothek wie Apache HttpClient einbinden. Hier ist ein Beispiel, wie Sie eine Webseite mit Java herunterladen können:

```Java
import java.io.IOException;
import java.net.URL;
import java.util.Scanner;

public class WebpageDownloader {
    public static void main(String[] args) throws IOException {
        // URL der herunterzuladenen Webseite
        String url = "https://www.example.com";

        // Die URL wird in ein URL-Objekt umgewandelt
        URL webpageURL = new URL(url);

        // Ein Scanner-Objekt wird verwendet, um den Inhalt der Webseite zu lesen
        Scanner scanner = new Scanner(webpageURL.openStream());
        String content = "";

        // Der Inhalt der Webseite wird Zeile für Zeile gelesen und in "content" gespeichert
        while(scanner.hasNext()) {
            content += scanner.nextLine();
        }

        // Der heruntergeladene Inhalt wird ausgegeben
        System.out.println(content);

        // Der Scanner wird geschlossen
        scanner.close();
    }
}
```

Das oben genannte Beispiel nutzt die Java-Standardbibliothek, indem es die `URL`- und `Scanner`-Klassen verwendet, um den Inhalt der Webseite zu lesen. Es ist auch möglich, eine externe Bibliothek wie Apache HttpClient einzubinden, um den Vorgang zu vereinfachen.

## Tiefentauchen:
Das Herunterladen von Webseiten ist eine grundlegende Funktion in der Webentwicklung und ermöglicht es Programmierern, dynamische Inhalte in ihre Anwendungen zu integrieren. Es gibt verschiedene Möglichkeiten, eine Webseite in Java herunterzuladen, einschließlich der Verwendung von externen Bibliotheken wie Apache HttpClient oder der Nutzung der Java-Standardbibliothek.

Weitere Informationen und Anwendungsbeispiele finden Sie in der offiziellen Java-Dokumentation und auf Websites wie Stack Overflow.

## Siehe auch:
- [Java Documentation - URL class](https://docs.oracle.com/javase/10/docs/api/java/net/URL.html)
- [Stack Overflow - How to download a webpage](https://stackoverflow.com/questions/238547/how-do-you-programmatically-download-a-webpage-in-java)