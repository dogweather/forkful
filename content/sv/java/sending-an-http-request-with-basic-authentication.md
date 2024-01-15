---
title:                "Sända en http-förfrågan med grundläggande autentisering"
html_title:           "Java: Sända en http-förfrågan med grundläggande autentisering"
simple_title:         "Sända en http-förfrågan med grundläggande autentisering"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

Att skicka HTTP-förfrågan med grundläggande autentisering är ett användbart verktyg för att säkert kommunicera med en server. Det möjliggör också för användare att få åtkomst till skyddade resurser som kräver autentisering.

## Hur man gör

För att skicka en HTTP-förfrågan med grundläggande autentisering i Java, följ dessa steg:

1. Importera nödvändiga paket:
```:Java
import java.net.HttpURLConnection;
import java.net.URL;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
```

2. Skapa en URL-objekt för den server som du vill kommunicera med:
```:Java
URL url = new URL("https://www.example.com");
```

3. Öppna en anslutning med hjälp av `HttpURLConnection`-objektet:
```:Java
HttpURLConnection con = (HttpURLConnection) url.openConnection();
```

4. Ange önskad metod (ex. GET, POST, PUT) och egenskaper för anslutningen:
```:Java
con.setRequestMethod("GET"); // Byt ut GET mot önskad metod
con.setRequestProperty("Authorization", "Basic " + getEncodedCredentials()); // Skicka med autentiseringsuppgifter
```

5. Skicka förfrågan och hantera responsen:
```:Java
int response_code = con.getResponseCode();

// Läs in svar från servern
BufferedReader in = new BufferedReader(
    new InputStreamReader(con.getInputStream()));

String line;
StringBuilder response = new StringBuilder();

// Lägg samman alla rader från servern till en sträng
while ((line = in.readLine()) != null) {
    response.append(line);
}
in.close();

// Skriv ut svaret från servern
System.out.println(response.toString());
```

För att kryptera användarnas autentiseringsuppgifter innan de skickas med förfrågan kan du använda Base64-kodning. För att enkelt implementera detta kan du använda Java's `Base64`-klass:
```:Java
// Kodar användarnas autentiseringsuppgifter med Base64
private String getEncodedCredentials() {
    String username = "användarnamn";
    String password = "lösenord";
    String credentials = username + ":" + password;
    byte[] encodedAuth = Base64.getEncoder().encode(credentials.getBytes(StandardCharsets.UTF_8));
    return new String(encodedAuth);
}
```

## Djupdykning

En HTTP-förfrågan med grundläggande autentisering består av en rubrik som tillhandahåller autentiseringsuppgifter i form av användarnamn och lösenord. Denna rubrik är en del av HTTP-protokollet och finns tillgänglig för alla webbservrar som stöder grundläggande autentisering.

När autentiseringsuppgifterna skickas med en förfrågan måste de först krypteras med Base64-kodning. Detta gör att uppgifterna inte är synliga för någon som övervakar nätverkstrafiken. Därför är det viktigt att använda en säker anslutning (HTTPS) när du skickar autentiseringsuppgifter.

## Se även

- [HTTP med Java](https://www.baeldung.com/java-http-request)
- [Basic Authentication i Java](https://www.javatpoint.com/java-http-url-connection)
- [Java's Base64-klass](https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html)