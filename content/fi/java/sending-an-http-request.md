---
title:                "Lähettämällä http-pyyntö"
html_title:           "Java: Lähettämällä http-pyyntö"
simple_title:         "Lähettämällä http-pyyntö"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi lähettää HTTP-pyynnön? Yksi syy voisi olla kommunikointi sovellusten välillä, esimerkiksi kun halutaan hakea tietoa ulkoisesta API:sta. 

## Kuinka

```Java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

public class HTTPRequestExample {

    public static void main(String[] args) throws IOException {

        // Luodaan URL-olio, joka sisältää halutun pyyntöosoitteen
        URL url = new URL("https://www.example.com/api/data");

        // Avataan yhteys
        HttpURLConnection con = (HttpURLConnection) url.openConnection();

        // Määritetään pyyntötyyppi ja lähetetään pyyntö
        con.setRequestMethod("GET");

        // Luodaan buffered reader lukuavun kanssa
        BufferedReader in = new BufferedReader(
                new InputStreamReader(con.getInputStream()));

        // Luetaan pyynnön vastaussanoma ja tulostetaan se näytölle
        String response;
        StringBuffer content = new StringBuffer();
        while ((response = in.readLine()) != null) {
            content.append(response);
        }
        in.close();
        System.out.println(content);

        // Suljetaan yhteys
        con.disconnect();
    }
}
```

**Esimerkkituloste:**
```html
<html>
  <head>
    <title>Example</title>
    <meta name="description" content="This is an example of an HTTP request">
  </head>
  <body>
    <h1>Hello world!</h1>
  </body>
</html>
```

## Syvällinen sukellus

HTTP-pyynnön lähettäminen käyttää Java-luokkia `java.net.URL` ja `java.net.HttpURLConnection` luomaan yhteyden URL-osoitteeseen ja lähettämään pyynnön valitulla pyyntötyypillä. Pyyntö voi sisältää myös parametreja, joita voi asettaa luomalla `java.net.URLConnection` ja käyttämällä `setRequestProperty()` -metodia. Yhteys tulee myös sulkea ja vastaussanoma lukea sulkemisen jälkeen. 

## Katso myös

- Oracle Java-tutoriaali: [HTTP-ja virheen käsittely](https://docs.oracle.com/javase/tutorial/networking/urls/readingWriting.html)
- w3schools: [Java HTTP-pyynnön lähettäminen](https://www.w3schools.com/java/java_httpurlconnection.asp)