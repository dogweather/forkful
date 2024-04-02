---
date: 2024-01-20 17:59:58.453227-07:00
description: "Wysy\u0142anie \u017C\u0105dania HTTP to komunikacja z serwerem w sieci;\
  \ mo\u017Cna prosi\u0107 o dane lub wysy\u0142a\u0107 je. Programi\u015Bci to robi\u0105\
  , by pobiera\u0107 informacje, autoryzowa\u0107\u2026"
lastmod: '2024-03-13T22:44:35.274252-06:00'
model: gpt-4-1106-preview
summary: "Wysy\u0142anie \u017C\u0105dania HTTP to komunikacja z serwerem w sieci;\
  \ mo\u017Cna prosi\u0107 o dane lub wysy\u0142a\u0107 je. Programi\u015Bci to robi\u0105\
  , by pobiera\u0107 informacje, autoryzowa\u0107\u2026"
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
weight: 44
---

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
