---
title:                "Java: Sända en http-förfrågan med grundläggande autentisering"
simple_title:         "Sända en http-förfrågan med grundläggande autentisering"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

Att skicka en HTTP-förfrågan med grundläggande autentisering är en viktig del av utvecklingen av webbapplikationer och API:er. Med grundläggande autentisering kan du säkerställa att endast auktoriserade användare har tillgång till dina resurser.

## Hur man

För att skicka en HTTP-förfrågan med grundläggande autentisering i Java behöver du först skapa en instans av klassen `HttpURLConnection` och ställa in dess URL-adress till den resurs du vill nå. Sedan behöver du inställningar för grundläggande autentisering, med användarnamn och lösenord. Slutligen behöver du specificera vilken HTTP-metod du vill använda, till exempel GET eller POST, och skicka förfrågan genom att använda `getInputStream()` eller `getOutputStream()` metoder.

Ett enkelt exempel på kod för att skicka en HTTP GET-förfrågan med grundläggande autentisering i Java ser ut så här:

```Java
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;

public class HttpBasicAuthenticationExample {
    public static void main(String[] args) throws IOException {
        // Skapa en URL för att ansluta till
        URL url = new URL("https://www.example.com/api/resources");

        // Skapa en nya HTTP-anslutning
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();

        // Ställ in autentiseringsinformation
        String username = "user";
        String password = "password";
        String basicAuth = "Basic " + Base64.getEncoder().encodeToString((username + ":" + password).getBytes());
        connection.setRequestProperty ("Authorization", basicAuth);

        // Specificera HTTP-metod
        connection.setRequestMethod("GET");

        // Skicka förfrågan och ta emot svar
        InputStream response = connection.getInputStream();

        // Skriva ut svaret
        int bytesRead;
        byte[] buffer = new byte[1024];
        while ((bytesRead = response.read(buffer)) > 0) {
            System.out.println(new String(buffer, 0, bytesRead));
        }
        response.close();
    }
}
```

Om allt går som planerat, bör du se en utskrift av svar i konsolen som är hämtat från den angivna URL-adressen.

## Djupdykning

Grundläggande autentisering fungerar genom att använda en Base64-kodad sträng av användarnamn och lösenord som sätts som en del av begäran HTTP-begäran. Detta tillhandahåller en grundläggande säkerhetsmekanism för att begränsa åtkomsten till dina resurser.

Du kan också använda andra autentiseringsmetoder som Signaturbaserad autentisering eller OAuth för en starkare säkerhet.

## Se även

* [Java HTTP-bibliotek](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
* [OAuth-autentisering i Java](https://www.baeldung.com/oauth-api-testing-with-spring-mvc)
* [HTTP-grundläggande autentiseringsstandard](https://www.rfc-editor.org/rfc/rfc2617.html)