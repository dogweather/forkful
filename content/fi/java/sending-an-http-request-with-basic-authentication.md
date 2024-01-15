---
title:                "Perusautentikoinnilla http-pyynnön lähettäminen."
html_title:           "Java: Perusautentikoinnilla http-pyynnön lähettäminen."
simple_title:         "Perusautentikoinnilla http-pyynnön lähettäminen."
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

Perusautentikaatiota käytetään HTTP-pyyntöjen lähettämisessä turvallisuuden varmistamiseksi. Se auttaa välttämään sivuston tai sovelluksen luvaton käyttö.

## Miten

```Java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

public class BasicAuthRequestExample {

    public static void main(String[] args) {
        try {
            // Luodaan URL ja muotoillaan käyttäjänimi ja salasana
            URL url = new URL("http://esimerkkisivusto.com");
            String username = "käyttäjänimi";
            String password = "salasana";
            
            // Avataan yhteys ja asetetaan pyynnölle perusautentikaatio
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("GET");
            String auth = username + ":" + password;
            String encodedAuth = Base64.getEncoder().encodeToString(auth.getBytes());
            connection.setRequestProperty("Authorization", "Basic " + encodedAuth);
            
            // Vastaanotetaan vastaus ja luetaan se
            BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            String line;
            StringBuffer response = new StringBuffer();
            while ((line = reader.readLine()) != null) {
                response.append(line);
            }
            reader.close();
            
            // Tulostetaan vastaus konsoliin
            System.out.println("HTTP statuskoodi: " + connection.getResponseCode());
            System.out.println("Vastaus: " + response.toString());
        } catch (Exception e) {
            // Tulostetaan mahdollinen virheilmoitus
            System.out.println("Virhe: " + e.getMessage());
        }

    }
}
```

Esimerkkikonsolitulos:

```
HTTP statuskoodi: 200
Vastaus: {"message":"Tervetuloa!"}
```

## Syventävä tieto

Perusautentikaatio on yksi yleisimmistä tavoista varmistaa HTTP-pyynnön lähettäjän aitous. Se toimii perustuen base64-koodaukseen, jossa käyttäjänimi ja salasana yhdistetään merkkijonona ja koodataan ennen pyynnön lähettämistä. Tämän ansiosta käyttäjätunnukset eivät ole pelkästään selkokielisessä muodossa, jolloin niitä on vaikeampi varastaa. Perusautentikaatio on myös helppo toteuttaa ja yleisesti tuettu eri ohjelmointikielissä ja työkaluissa.

## Katso myös

- [Java HttpURLConnection documentation](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Base64 Java documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html)
- [Basic Authentication article by HTTPwatch](https://blog.httpwatch.com/2009/06/17/http-authentication-dialogs-decoded/)