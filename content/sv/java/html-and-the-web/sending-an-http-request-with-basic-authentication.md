---
date: 2024-01-20 18:02:15.914307-07:00
description: "Hur man g\xF6r: Sample output saknas h\xE4r eftersom det handlar mer\
  \ om serverns respons \xE4n sj\xE4lva Java-koden."
lastmod: '2024-04-05T21:53:39.120699-06:00'
model: gpt-4-1106-preview
summary: "Sample output saknas h\xE4r eftersom det handlar mer om serverns respons\
  \ \xE4n sj\xE4lva Java-koden."
title: "Skicka en HTTP-f\xF6rfr\xE5gan med Basic-autentisering"
weight: 45
---

## Hur man gör:
```java
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Base64;

public class BasicAuthRequest {

    public static void main(String[] args) throws IOException {
        String webPage = "http://example.com/api";
        String name = "user";
        String password = "pass";

        String authString = name + ":" + password;
        String encodedAuth = Base64.getEncoder().encodeToString(authString.getBytes());

        URL url = new URL(webPage);
        HttpURLConnection urlConnection = (HttpURLConnection) url.openConnection();
        urlConnection.setRequestProperty("Authorization", "Basic " + encodedAuth);

        // Resten av koden för att hantera anslutningens input/output kommer här...
    }
}
```

Sample output saknas här eftersom det handlar mer om serverns respons än själva Java-koden.

## Djupdykning
Skicka HTTP-förfrågan med grundläggande autentisering är så elementärt som det låter - det har använts sedan HTTP/1.0. Även om det är enkelt, bör man vara medveten om att det inte är det säkraste sättet att skicka känslig information eftersom bas64-kodning lätt kan dekodas. Modernt alternativ inkluderar OAuth och tokens.

En viktig aspekt av implementationen i Java är `HttpURLConnection`-klassen, som möjliggör skapandet och hanteringen av förbindelser till resurser på nätverket. Från Java 11 och framåt kan man även använda `HttpClient`-klassen för att göra HTTP-förfrågningar på ett mer flexibelt sätt, inklusive asynkron hantering.

## Se också
- [Java 11 HttpClient Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [Base64 Encoding and Decoding](https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html)
- [RFC 7617 'The 'Basic' HTTP Authentication Scheme'](https://tools.ietf.org/html/rfc7617)
- [Understanding Java HttpURLConnection](https://www.baeldung.com/java-http-url-connection)
