---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?

HTTP-request med Basic Authentication innebär att skicka en begäran till en server med användarnamn och lösenord för säker åtkomst. Programmerare gör det för att skydda känslig information och begränsa åtkomst till nödvändiga användare.

## Hur man gör:

Här är ett grundläggande exempel på hur man skickar en HTTP-begäran med Basic Authentication i Java:

```Java
import java.net.URL;
import java.net.HttpURLConnection;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

public class BasicAuthRequest {
    public static void main(String[] args) throws Exception {
        URL url = new URL("http://example.com");
        
        String userCredentials = "username:password";
        String basicAuth = "Basic " + new String(Base64.getEncoder().encode(userCredentials.getBytes(StandardCharsets.UTF_8)));

        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setRequestProperty("Authorization", basicAuth);

        int responseCode = connection.getResponseCode();
        System.out.println("Response Code : " + responseCode);
    }
}
```

Exemplet ovan skapar en HTTP-anslutning till `http://example.com` med användaruppgifter kodade i Basic Authentication-stilen.

## Djupdykning

1. Historisk kontext: Basic Authentication inrättades ursprungligen som en del av HTTP / 1.0-standarden på 90-talet. Det är inte lika säkert som nyare autentiseringsmetoder, men det är fortfarande populärt för sin enkelhet.
2. Alternativ: Det finns säkrare alternativ till Basic Authentication tillgängliga, såsom OAuth och Kerberos. 
3. Implementeringsdetaljer: När du skickar en HTTP-begäran med Basic Authentication, följer användarnamnet och lösenordet med i begäran. Denna information kodas dock med Base64, vilket inte är en krypteringsmetod, endast en kodning. 

## Se även

1. Officiell Java-dokumentation: [URL-klassen](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/net/URL.html)
2. Officiell Java-dokumentation: [HttpURLConnection-klassen](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/net/HttpURLConnection.html)
3. [Basic Authentication på Wikipedia](https://sv.wikipedia.org/wiki/Basic_access_authentication)
4. [Alternativa autentiseringsmetoder](https://auth0.com/docs/protocols)