---
title:                "Wysyłanie żądania HTTP"
aliases: - /pl/java/sending-an-http-request.md
date:                  2024-01-20T17:59:58.453227-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie żądania HTTP to komunikacja z serwerem w sieci; można prosić o dane lub wysyłać je. Programiści to robią, by pobierać informacje, autoryzować użytkowników, czy integrować usługi.

## Jak to zrobić:
Wysyłanie prostej żądania GET w Javie:
```java
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.URI;

public class Main {
    public static void main(String[] args) throws Exception {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create("http://example.com"))
                .build();

        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
        System.out.println(response.statusCode());
        System.out.println(response.body());
    }
}
```
Wynik:
```
200
<!doctype html>...
```

## Dogłębna analiza
HTTP to protokół sieciowy od 1991 roku. Java oferuje kilka metod do żądań: `HttpURLConnection`, starsze biblioteki jak `Apache HttpClient`, `OkHttp`, a od Javy 11 - nowe API `java.net.http`. Nowe API to bardziej czytelny kod i lepsza obsługa asynchroniczności.

## Zobacz też:
- Specyfikacja HTTP: https://www.ietf.org/rfc/rfc2616.txt
- Dokumentacja Oracle dla `java.net.http`: https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/package-summary.html
- Porównanie klientów HTTP w Javie: https://www.baeldung.com/java-9-http-client vs-httpurlconnection
