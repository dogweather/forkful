---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTTP-pyynnön lähettäminen perusautentikoinnilla tarkoittaa sitä, että ohjelmistokehittäjä voi toteuttaa turvallisen viestin siirron järjestelmäkomponenttien välillä webissä. Tätä menetelmää käytetään laajasti, koska se suojaa arkaluontoiset tiedot verkkorikollisilta.

## Miten:

Java tarjoaa HttpUrlConnection -luokan, jolla voit lähettää HTTP-pyynnön perusautentikoinnilla. Tässä on esimerkkikoodi:

```Java
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;

public class Main {
    public static void main(String[] args) {
        try {
            URL url = new URL("http://www.example.com");
            String userCredentials = "username:password";
            String basicAuth = "Basic " + javax.xml.bind.DatatypeConverter.printBase64Binary(userCredentials.getBytes());

            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("POST");
            connection.setRequestProperty("Authorization", basicAuth);
            connection.setDoOutput(true);

            OutputStream output = connection.getOutputStream();
            output.write("message content".getBytes());
            output.flush();
            output.close();
            
            int responseCode = connection.getResponseCode();
            System.out.println("Response Code : " + responseCode);

            connection.disconnect();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
Koodi luo URL-olion osoitteella "http://www.example.com". Käyttäjätunnukset ja salasana koodataan Base64-muotoon, joka asetetaan pyynnön otsikkoon. Tämän jälkeen on mahdollista lähettää viesti, tässä tapauksessa "message content".

Tulostaa: Response Code : 200

## Deep Dive:

Perusautentikointi on ollut olemassa HTTP-protokollasta lähtien ja se on yksi yksinkertaisimmista tavoista suojata web-resurssit. Se ei salaa tietoja, vaan koodaa ne Base64-muotoon, joten salasanan voi paljastaa, jos joku sieppaa verkkoliikenteen. Siksi käytä HTTPS:tä aina kun mahdollista.

Vaihtoehtoja on lukuisia. OAuth ja JWT (JSON Web Token) ovat kaksi yleisintä menetelmää, jotka tarjoavat paremman turvallisuuden ja joustavamman käytön.

Lopuksi, Java tarvitsee selkeän Base64-koodatun merkkijonon, joka sisältää käyttäjänimen ja salasanan muodossa "username:password". Tämä merkkijono asetetaan HTTP-otsikkoon käytettäessä "Basic" ennen koodattua merkkijonoa.

## Katso Myös:

- [HttpURLConnection Documentation](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Basic Authentication on Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [OAuth Homepage](https://oauth.net/)
- [JWT.io Introduction](https://jwt.io/introduction/)