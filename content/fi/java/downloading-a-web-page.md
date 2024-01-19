---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Lataamalla verkkosivu on tapa tuoda verkkosivun tiedot tietokoneellesi koodin avulla. Tekijät tekevät tämän, jotta he voivat käsitellä sivun tietoja automaattisesti, esimerkiksi web-skraping- tai datanlouhintahankkeissa.

## Miten tehdään?

Tässä on yksinkertainen Java-koodi, joka lataa verkkosivun:

```Java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;

public class Downloader {
    public static void main(String[] args) throws Exception {
        URL url = new URL("http://www.example.com"); 
        BufferedReader br = new BufferedReader(new InputStreamReader(url.openStream()));
        String line;
        
        while ((line = br.readLine()) != null) {
            System.out.println(line);
        }
    }
}
```
Kun suoritat tämän koodin, se näyttäisi jotakin alla olevaa vastaavaa:

```Java
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```
##Erittäin yksityiskohtaisesti

Aikoinaan, kun WWW (World Wide Web) oli vasta syntymässä, web-sivuja ei voitu ladata automaattisesti koodilla. Nykypäivänä meillä on paljon erilaisia keinoja ja työkaluja, kuten yllä mainittu Java-esimerkki.

Java-tiedoston streaming-luokkien avulla pystymme lataamaan web-sivuja, mutta on myös olemassa muita vaihtoehtoja, kuten HttpClient ja Jsoup-kirjasto. Nämä mahdollistavat entistä monimutkaisempien tehtävien tekemisen, kuten evästeiden hallinnan ja Javascriptin manipuloinnin.

Merkittävä yksityiskohta verkkosivujen lataamisessa on URL-yhteyden avaaminen ja sen lukeminen strategisesti puskuroivan lukijan avulla. Tämän avulla voimme käsitellä suurempia tietomääriä ilman muistin ylikuormittumista.

## Katso myös

- Oracle Java-dokumentaatio: [Lue verkkoresurssista](https://docs.oracle.com/javase/tutorial/networking/urls/readingURL.html)
- Apache HttpClient: [Käyttöohje](https://hc.apache.org/httpcomponents-client-4.5.x/tutorial/html/fundamentals.html)
- Jsoup: [Java HTML Parserin dokumentaatio](https://jsoup.org/)