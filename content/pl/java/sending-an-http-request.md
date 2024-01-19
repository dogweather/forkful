---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysłanie żądania HTTP to proces, w którym program komunikuje się z serwerem za pomocą protokołu HTTP. Programiści robią to, aby pobierać dane, wysyłać dane, usuwać dane itp. na serwerach.

## Jak to zrobić:

```Java
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.URI;

public class Main {
    public static void main(String[] args) throws Exception {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
              .uri(new URI("http://example.com"))
              .GET()
              .build();
        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
        System.out.println(response.body());
    }
}
```
Przykładowe dane wyjściowe:

```Java
<!doctype html>
<html>
...
</html>
```
## Deep Dive

Historia: Protokół HTTP został początkowo wprowadzony w 1991 roku jako standard komunikacji internetowej.

Alternatywy: Istnieją inne protokoły, takie jak FTP, HTTPS, itp.

Szczegóły implementacji: Możemy wysyłać rozmaite typy żądań HTTP, takie jak GET, POST, DELETE, PUT, itp. HttpClient dostarcza funkcję 'send', która wysyła żądanie do serwera i zwraca odpowiedź.

## Zobacz także

1. [Dokumentacja Oracle o HttpClient](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
2. [Wyjaśnienie protokołu HTTP](https://developer.mozilla.org/pl/docs/Web/HTTP/Overview)
3. [Jak wysłać żądanie HTTP POST w Javie - Poradnik](https://www.baeldung.com/java-http-request)