---
date: 2024-01-20 18:01:46.944705-07:00
description: "How to: (Kuinka tehd\xE4:) ."
lastmod: '2024-03-13T22:44:56.444631-06:00'
model: gpt-4-1106-preview
summary: .
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla"
weight: 45
---

## How to: (Kuinka tehdä:)
```java
import java.io.IOException;
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.URL;
import javax.net.ssl.HttpsURLConnection;
import java.util.Base64;

public class BasicAuthRequest {
    public static void main(String[] args) {
        try {
            URL url = new URL("https://example.com/api/data");
            String encoding = Base64.getEncoder().encodeToString(("user:password").getBytes());

            HttpsURLConnection connection = (HttpsURLConnection) url.openConnection();
            connection.setRequestMethod("GET");
            connection.setDoOutput(true);
            connection.setRequestProperty("Authorization", "Basic " + encoding);

            int responseCode = connection.getResponseCode();
            System.out.println("Response Code: " + responseCode);

            // Handle input stream (response) as necessary...

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
Sample output:
```
Response Code: 200
```

## Deep Dive (Syväsukellus):
Perusautentikaatio on internetin varhainen turvallisuusmekanismi, joka edelleen suojelee resursseja. Muita menetelmiä ovat OAuth ja JWT (JSON Web Tokens). Tämä koodi luo `Authorization` otsikon Base64-koodatusta merkkijonosta, joka on muotoiltu "käyttäjätunnus:salasana". Ohjelma avaa yhteyden resurssiin ja välittää koodatun tunnistetiedon palvelimelle.

## See Also (Katso Myös):
- [Java HttpURLConnection Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/net/HttpURLConnection.html)
- [Base64 Encoding in Java](https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [MDN Web Docs - Authorization](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
