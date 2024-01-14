---
title:                "Java: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

HTTP-forespørsler med grunnleggende autentisering er vanligvis brukt når en bruker vil identifisere seg selv og få tilgang til en sikret ressurs, for eksempel en nettbankkonto eller en e-postkonto. Dette er en vanlig måte å beskytte sensitive data og sikre at bare autoriserte brukere får tilgang.

## Hvordan

Det første trinnet for å sende en HTTP-forespørsel med grunnleggende autentisering er å sette opp en tilkobling til målserveren ved hjelp av `java.net.URL`-klassen. Deretter må tilkoblingen åpnes ved hjelp av `openConnection()`-metoden. Når tilkoblingen er åpnet, kan vi sette opp forespørselsmetoden og eventuelle tilleggsparametere ved hjelp av `setRequestMethod()` og `setRequestProperty()`-metodene.

Deretter må vi legge til brukernavn og passord i forespørselen ved hjelp av `setRequestProperty()`-metoden og angi `Authorization`-headeren til `Basic` etterfulgt av base64-kodet brukernavn og passord separert med et kolon (f.eks. `username:password`). Til slutt må vi kalle `getInputStream()`- eller `getOutputStream()`-metodene for å utføre forespørselen og lese eller skrive data.

Her er et eksempel på kode for å sende en HTTP GET-forespørsel med grunnleggende autentisering:

```java
try {
    URL url = new URL("https://example.com/api/resource");
    HttpURLConnection connection = (HttpURLConnection) url.openConnection();

    // Set request method and properties
    connection.setRequestMethod("GET");
    connection.setRequestProperty("Authorization", "Basic dXNlcm5hbWU6cGFzc3dvcmQ="); // Replace with base64 encoded username:password

    // Execute request and read response
    InputStream response = connection.getInputStream();
    BufferedReader reader = new BufferedReader(new InputStreamReader(response));
    String output = reader.readLine();
    System.out.println(output);

    // Close connections
    reader.close();
    connection.disconnect();
} catch (IOException e) {
    e.printStackTrace();
}
```

Dette eksempelet bruker `java.net.HttpURLConnection`-klassen, men det er også mulig å bruke tredjeparts biblioteker som Apache HttpComponents eller OkHttp for å forenkle koden.

## Deep Dive

Den `Authorization`-headeren som brukes i grunnleggende autentisering inneholder en base64-kodet verdi av brukernavn og passord separert med et kolon. Base64-koding er ikke en form for kryptering og kan enkelt dekodes, derfor bør dette bare brukes som et grunnleggende sikkerhetstiltak og ikke som en fullstendig sikkerhetsløsning.

Det finnes også alternative måter å autentisere HTTP-forespørsler på, som for eksempel digest og OAuth. Disse metodene er mer komplekse og gir bedre sikkerhet, men er utenfor omfanget av denne bloggartikkelen.

## Se Også

- [HTTP-forespørsler og serverkoblinger i Java](https://docs.oracle.com/javase/tutorial/networking/urls/readingWriting.html)
- [Apache HttpComponents](https://hc.apache.org/)
- [OkHttp](https://square.github.io/okhttp/)