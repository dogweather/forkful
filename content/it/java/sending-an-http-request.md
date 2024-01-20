---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

L'invio di una richiesta HTTP è un modo per piantare una bandiera di dialogo tra il tuo computer (il client) e un server web. I programmatori lo fanno per comunicare con le API web, scaricare file e molto altro.

## Come fare:

Ecco un esempio semplice di come inviare una richiesta GET usando la libreria java.net.http di Java 11.

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

        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

        System.out.println(response.body());
    }
}
```

Quando esegui questo codice, vedrai un output di stringa nel terminale.

## Approfondimento:

Java offre supporto per l'invio di richieste HTTP da molto tempo, inizialmente con l'antiquato `java.net.URLConnection`, poi con `java.net.HttpURLConnection` e ora con il più recente `java.net.http.HttpClient`. 

Se non stai utilizzando Java 11 o una versione successiva, potresti dover utilizzare una libreria esterna come Apache HttpClient o OkHttp per avere le stesse capacità. 

Quando si invia una richiesta HTTP, è possibile modificare una serie di dettagli come tipo di richiesta (GET, POST, etc.), intestazioni, payload del corpo e molto altro.

## Vedi Anche:

- La documentazione API ufficiale di Java per HttpClient: https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html
- Un tutorial più completo sull'utilizzo di HttpClient: https://www.baeldung.com/java-11-http-client
- Un confronto tra le diverse librerie HTTP in Java: https://www.baeldung.com/java-9-http-client