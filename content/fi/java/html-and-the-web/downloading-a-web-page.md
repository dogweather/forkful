---
title:                "Verkkosivun lataaminen"
aliases:
- /fi/java/downloading-a-web-page/
date:                  2024-01-20T17:44:22.156762-07:00
model:                 gpt-4-1106-preview
simple_title:         "Verkkosivun lataaminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Lataamme verkkosivun sisältöä tutkiaksemme sen rakennetta tai hyödyntääksemme tietoja. Ohjelmoijat tekevät tätä esimerkiksi datankeruuseen, palveluiden integraatioon tai sisällön seurantaan.

## How to: - Kuinka:
```java
import java.io.*;
import java.net.*;

public class WebPageDownloader {
    public static void main(String[] args) {
        String webPageUrl = "http://example.com";
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(new URL(webPageUrl).openStream()))) {
            String line;
            while ((line = reader.readLine()) != null) {
                System.out.println(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
Tuloste olisi verkkosivun HTML-koodi, esimerkiksi:
```html
<!doctype html>
<html>
<head>
    <title>Esimerkki</title>
</head>
<body>
    <p>Tämä on esimerkki verkkosivu.</p>
</body>
</html>
```

## Deep Dive - Syväsukellus:
Web-sivujen lataaminen on yleinen tarve jo 90-luvulta lähtien. Aluksi käytettiin perinteisiä soketteja, mutta Java 1.0 esitteli `URL` ja `URLConnection` luokat tuon tarpeen helpottamiseksi. Nykyään on olemassa useita kirjastoja, kuten Jsoup ja Apache HttpClient, jotka tarjoavat lisäominaisuuksia ja helpottavat virheenkäsittelyä.

Java 11 toi mukanaan uuden HTTP Clientin, joka tukee HTTP/2 ja modernia asynkronista ohjelmointia. Se on tärkeää muistaa, kun puhutaan suorituskyvystä ja suuremmista järjestelmistä.

Sivun lataamisen oikeellisuus ja sen käyttäytymisen hallinta - kuten käyttäjä-agentin määrittäminen, evästeiden käsittely, ja redirectien seuranta - ovat tärkeitä tekijöitä implementaatiossa.

## See Also - Katso Myös:
- Jsoup (https://jsoup.org/) - HTML:n parsiintiin ja käsittelyyn.
- Apache HttpClient (https://hc.apache.org/httpcomponents-client-5.1.x/index.html) - Monipuolinen HTTP-asiakaskirjasto.
- OpenJDK:n HTTP Client (https://openjdk.java.net/groups/net/httpclient/intro.html) - Moderni HTTP-asiakas tiedon lataamiseen.
