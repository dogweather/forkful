---
title:                "Sivun lataaminen"
html_title:           "Java: Sivun lataaminen"
simple_title:         "Sivun lataaminen"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Web-sivun lataaminen on tärkeä osa nykypäivän ohjelmointia, joka mahdollistaa tietojen hakemisen ja käsittelemisen internetissä. Java tarjoaa laajan valikoiman työkaluja ja kirjastoja, jotka helpottavat web-sivujen lataamista ja niiden sisällön käsittelyä.

## Kuinka

```Java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;

public class WebpageDownloader {
  
  public static void main(String[] args) {
    
    // Luodaan URL-objekti halutulle web-sivulle
    URL url = new URL("https://www.example.com");
    
    // Avataan virta web-sivulle
    InputStream is = url.openStream();
    
    // Luodaan lukija lukemaan web-sivun sisällön
    BufferedReader br = new BufferedReader(new InputStreamReader(is));
    
    String line = null;
    StringBuilder sb = new StringBuilder();
    
    // Luetaan web-sivun sisältö rivi riviltä ja tallennetaan StringBuilderiin
    while ((line = br.readLine()) != null) {
      sb.append(line);
    }
    
    // Tulostetaan web-sivun sisältö konsolille
    System.out.println(sb.toString());
    
    // Suljetaan lukija ja virta
    br.close();
    is.close();
  }
}
```

Esimerkkituloste:
```
<!DOCTYPE html>
<html>
<head>
  <title>Example Domain</title>
  <meta charset="utf-8" />
</head>
<body>
<div>
  <h1>Example Domain</h1>
  <p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p>
  <p><a href="https://www.iana.org/domains/example">More information...</a></p>
</div>
</body>
</html>
```

## Syvempi sukellus

Java tarjoaa useita erilaisia työkaluja ja kirjastoja web-sivujen lataamiseen ja niiden sisällön käsittelyyn. Näitä ovat mm. luokat URL, URLConnection, BufferedReader ja InputStream. Näiden avulla voidaan avata yhteys haluttuun web-sivuun ja lukea sen sisältöä rivi riviltä. Lisäksi Java tarjoaa erilaisia metodeja, kuten get, post ja put, joilla voidaan lähettää ja vastaanottaa tietoa web-sivun kautta.

## Katso myös

- [Oracle Java-Dokumentaatio](https://docs.oracle.com/javase/10/docs/api/java/net/URL.html)
- [Stack Overflow -kysymys ja vastaus web-sivun lataamisesta Javalla](https://stackoverflow.com/questions/238547/how-do-you-programmatically-download-a-webpage-in-java)