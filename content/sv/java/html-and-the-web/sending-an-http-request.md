---
date: 2024-01-20 18:00:00.820896-07:00
description: "Att skicka en HTTP-beg\xE4ran inneb\xE4r att du som programmerare fr\xE5\
  gar en server om information eller ber den utf\xF6ra en \xE5tg\xE4rd. Det \xE4r\
  \ kritiskt f\xF6r\u2026"
lastmod: 2024-02-19 22:04:56.992812
model: gpt-4-1106-preview
summary: "Att skicka en HTTP-beg\xE4ran inneb\xE4r att du som programmerare fr\xE5\
  gar en server om information eller ber den utf\xF6ra en \xE5tg\xE4rd. Det \xE4r\
  \ kritiskt f\xF6r\u2026"
title: "Skicka en http-f\xF6rfr\xE5gan"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran innebär att du som programmerare frågar en server om information eller ber den utföra en åtgärd. Det är kritiskt för webbutveckling, API-interaktion och automatisering av webbuppgifter.

## Hur gör man:
Java's `HttpClient` gör det enkelt att skicka HTTP-begäran. Här är ett snabbt exempel för att skicka en GET-begäran och skriva ut svaret:

```java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class SimpleHttpClient {
    public static void main(String[] args) throws Exception {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                .uri(new URI("http://example.com"))
                .GET()
                .build();
        
        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

        System.out.println(response.statusCode());
        System.out.println(response.body());
    }
}
```

Kör koden och få svaret:

```
200
<!doctype html>...
```

## Djupdykning:
HTTP-begäran har funnits sedan webbens födelse, först specificerad i 1991. Nu används HTTP/2 och HTTP/3 för snabbare prestanda. Alternativ till Javas `HttpClient` inkluderar bibliotek som `Apache HttpClient` och `OkHttp`. Dessa erbjuder fler funktioner, men Java's inbyggda klient är tillräcklig för enkel användning. Implementeringsdetaljer är viktiga; ställ in timeouts, hantera olika HTTP-metoder, hantera omdirigeringar, och kontrollera svar för att bygga robusta applikationer.

## Se även:
- Oracle officiella dokumentation på `HttpClient`: [https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- Mozilla Developer Network på HTTP: [https://developer.mozilla.org/en-US/docs/Web/HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
