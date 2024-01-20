---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Herunterladen einer Webseite ist der Prozess, bei dem ein Programm die Inhalte einer Webseite aus dem Internet abruft und lokal speichert. Programmierer tun dies, um Webdaten zu analysieren, Ressourcen zu speichern oder um Offlinemöglichkeiten zu ermöglichen.

## So geht's:
Unten finden Sie ein einfaches Beispiel, wie Sie eine Webseite mit Java und der Bibliothek `java.net.HttpURLConnection` herunterladen können:

```Java
import java.net.HttpURLConnection;
import java.net.URL;
import java.io.BufferedReader;
import java.io.InputStreamReader;

public class Main {
    public static void main(String[] args) throws Exception {
        
        // URL definieren
        URL url = new URL("http://beispiel.de");

        // Verbindung öffnen
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();

        // Response lesen
        BufferedReader reader = new BufferedReader(new InputStreamReader(conn.getInputStream()));
        String line;
        while ((line = reader.readLine()) != null) {
            System.out.println(line);
        }
        reader.close();
    }
}
```
Dieses Programm wird die gesamte HTML-Daten einer gegebenen URL ausdrucken.

## Vertiefung
Historisch gesehen, wurden Webseiten massenhaft heruntergeladen, um Webindizes für Suchmaschinen wie Google zu erstellen. Heute wird dies auch oft zur Datenanalyse (Web Scraping) oder zur Archivierung verwendet.

Es gibt viele Alternativen zum Herunterladen von Webseiten in Java, z.B. die Bibliotheken Jsoup oder HTMLUnit. Diese bieten weitere Funktionen, wie das Durchsuchen und Analysieren von HTML-Daten.

Die Implementierungsdetails, insbesondere die Behandlung von HTTP-Response-Codes, Umleitungen und Cookies, hängen von den spezifischen Anforderungen deines Programms ab.

##  Siehe auch
Weitere nützliche Ressourcen:

1. Jsoup: https://jsoup.org/
2. HTMLUnit: http://htmlunit.sourceforge.net/
3. Java HttpURLConnection Dokumentation: https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html
4. Mehr über Web Scraping: https://de.wikipedia.org/wiki/Screen_Scraping

Erinnere dich immer an gute Praktiken beim Herunterladen von Webseiten, respektiere immer die Nutzungsbedingungen und Datenschutzrichtlinien der Webseiten, die du herunterlädst.