---
title:                "Att skicka en http-begäran"
html_title:           "Go: Att skicka en http-begäran"
simple_title:         "Att skicka en http-begäran"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?

HTTP-begäran är en standardmetod för att kommunicera information på webben. Programmerare skickar HTTP-begäran för att interagera med webbsidor, hämta data, posta data och mycket mer. 

## Hur man gör:

Här är en enkel Java-kod för att skicka en GET-request:

```Java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class Main {
    public static void main(String[] args) throws Exception {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
            .uri(new URI("http://example.com"))
            .build();

        HttpResponse<String> response = 
            client.send(request, HttpResponse.BodyHandlers.ofString());

        System.out.println(response.body());
    }
}
```

När du kör denna kod, kommer output att vara HTML-innehållet i webbplatsen http://example.com

## Djupdykning

Sedan HTTP introducerades 1991, har metoden för att skicka HTTP-begäran förändrats. I Java, före version 11, användes `HttpURLConnection`. Java 11 introducerade `HttpClient` som är mer effektiv och lättare att använda.

Alternativ till Java inkluderar programmeringsspråk som Python och libraries som Apache HttpClient. 

Att skicka en HTTP-begäran involverar flera steg: att skapa en `HttpClient`, bygga en `HttpRequest`, skicka begäran och fånga svaret. Varje steg kan anpassas efter dina behov.

## Se även

- [Java 11 HttpClient dokumentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [Mozilla Developer Network HTTP-guide](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- [Apache HttpClient](http://hc.apache.org/httpcomponents-client-4.5.x/index.html)