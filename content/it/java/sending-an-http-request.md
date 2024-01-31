---
title:                "Inviare una richiesta http"
date:                  2024-01-20T18:00:00.371487-07:00
model:                 gpt-4-1106-preview
simple_title:         "Inviare una richiesta http"

category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Inviare una richiesta HTTP significa comunicare con un server web per scambiare dati. I programmatori lo fanno per interagire con servizi web, API e scaricare contenuti.

## How to: (Come fare:)
```java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class HttpExample {
    public static void main(String[] args) {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
            .uri(URI.create("https://api.example.com/data"))
            .build();
        
        client.sendAsync(request, HttpResponse.BodyHandlers.ofString())
            .thenApply(HttpResponse::body)
            .thenAccept(System.out::println)
            .join();
    }
}
```
Output:
```
{"name":"John Doe","occupation":"Java Developer"}
```

## Deep Dive (Approfondimento)
Il concetto di invio di richieste HTTP esiste da quando il protocollo HTTP è stato introdotto all'inizio degli anni '90. In Java, abbiamo assistito a un'evoluzione, da `HttpURLConnection` fino alla recente `HttpClient` nell'API java.net.http, introdotta in Java 9 e migliorata nelle versioni successive, che semplifica le operazioni asincrone e supporta HTTP/2.

Alternatives (Alternative):
- `HttpURLConnection`: meno moderno, più verboso.
- Librerie terze parti: OkHttp, Apache HttpClient.

Dettagli implementativi:
- `HttpClient`: supporta richieste GET, POST, PUT, DELETE, e altri metodi HTTP.
- `HttpRequest`: costruisce la richiesta. Definisce URI, metodi, headers, e body.
- `HttpResponse`: gestisce la risposta. Puoi recuperare status, headers e body.

## See Also (Vedi Anche)
- Documentazione ufficiale di `HttpClient`: [https://docs.oracle.com/en/java/javase/17/docs/api/java.net.http/java/net/http/HttpClient.html](https://docs.oracle.com/en/java/javase/17/docs/api/java.net.http/java/net/http/HttpClient.html)
- Guida alle richieste HTTP con Java: [https://www.baeldung.com/java-9-http-client](https://www.baeldung.com/java-9-http-client)
- API di OkHttp: [https://square.github.io/okhttp/](https://square.github.io/okhttp/)
