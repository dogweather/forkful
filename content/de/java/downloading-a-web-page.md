---
title:                "Herunterladen einer Webseite"
html_title:           "Java: Herunterladen einer Webseite"
simple_title:         "Herunterladen einer Webseite"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man eine Webseite herunterladen? Ganz einfach: um lokale Kopien von Inhalten zu haben oder um Web Scraping durchzuführen.

## Wie

Um eine Webseite in Java herunterzuladen, benötigt man die `java.net` Bibliothek. Hier ist ein Beispiel, wie man eine Webseite mit dem URL-Objekt herunterladen kann und den Inhalt in der Konsole ausgibt:

```Java
import java.io.*;
import java.net.*;

public class WebPageDownloader {
    public static void main(String[] args) throws IOException{
        URL url = new URL("https://www.example.com");
        BufferedReader in = new BufferedReader(new InputStreamReader(url.openStream()));
        String inputLine;
        while ((inputLine = in.readLine()) != null)
            System.out.println(inputLine);
        in.close();
    }
}
```

Die Ausgabe sieht dann so aus:

```
<html>
<head><title>Example Domain</title></head>
<body>
    <h1>Example Domain</h1>
    <p>This domain is for use in illustrative examples in documents. You may use
    this domain in literature without prior coordination or asking for
    permission.</p>
    <p><a href="https://www.iana.org/domains/example">More information...</a></p>
</body>
</html>
```

Man kann auch die `URLConnection` verwenden, um spezifische Header oder Anfragemethoden anzugeben. Hier ist ein Beispiel, wie man einen Benutzer-Agenten hinzufügt:

```Java
import java.io.*;
import java.net.*;

public class WebPageDownloader {
    public static void main(String[] args) throws IOException{
        URL url = new URL("https://www.example.com");
        URLConnection con = url.openConnection();
        con.addRequestProperty("User-Agent", "My User Agent");
        BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
        String inputLine;
        while ((inputLine = in.readLine()) != null)
            System.out.println(inputLine);
        in.close();
    }
}
```

Die Ausgabe bleibt die gleiche wie zuvor, aber diesmal wurde die URL-Anfrage mit dem angegebenen Benutzer-Agenten durchgeführt.

## Deep Dive

Das Herunterladen einer Webseite ist eigentlich der Retrieval von HTTP- oder HTTPS-Ressourcen. Man kann also nicht nur HTML-Dateien herunterladen, sondern auch Bilder, PDFs oder andere Dateiformate. Durch die Verwendung von `InputStream`- und `OutputStream`-Klassen kann man Dateien herunterladen und speichern, oder sogar direkt in ein Java-Objekt parsen.

Um mehr über die Möglichkeiten des Herunterladens von Webseiten in Java zu erfahren, kann man sich mit den verschiedenen Klassen in der `java.net` Bibliothek wie `URL`, `URLConnection` oder `HttpURLConnection` beschäftigen. Es gibt auch verschiedene Java-Bibliotheken von Drittanbietern, die nützliche Funktionen für das Herunterladen von Webseiten anbieten.

## Siehe Auch

- [Oracle Java Tutorial: Working with URLs](https://docs.oracle.com/javase/tutorial/networking/urls/)
- [Baeldung Java: How to Send HTTP Requests using java.net](https://www.baeldung.com/java-http-request)