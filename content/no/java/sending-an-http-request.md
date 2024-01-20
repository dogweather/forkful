---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel er prosessen hvor datamaskinen din ber en server om informasjon eller utfører en handling, som å laste opp en fil. Dette er essensielt for samhandlingen mellom klienter (for eksempel webleseren din) og servere.

## Hvordan:

Her er et eksempel på hvordan du kan sende en GET-forespørsel og skrive ut responsen i Java:

```Java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class Main {
    public static void main(String[] args) throws Exception {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create("http://example.com"))
                .build();

        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

        System.out.println(response.body());
    }
}
```

Når du kjører dette programmet, vil det sende en HTTP GET-forespørsel til `http://example.com` og så skrive ut responsen den mottar.

## Dypdykke:

1. Historisk kontekst: HTTP-forespørsler har vært en del av internettpraksis siden utgivelsen av HTTP/1.0 rundt 1996. De har blitt raffinert og utvidet med senere versjoner av protokollen, spesielt HTTP/2 og HTTP/3.  

2. Alternativer: Det finnes mange biblioteker der ute for å sende HTTP-forespørsler i Java, inkludert OkHttp og Apache HttpClient, men i denne artikkelen fokuserer vi på HttpClient som er innebygd i Java.

3. Gjennomføringsdetaljer: `HttpClient.newHttpClient()` brukes til å opprette en ny HttpClient. `HttpRequest.newBuilder()` starter konstruksjonen av en HttpRequest, og `.build()` fullfører det. Da sender vi forespørselen med `client.send()` og printer ut responsen.

## Se Også:

1. [Java 11 HttpClient Dokumentasjon](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/net/http/HttpClient.html)
2. [Mozilla HTTP Forespørsler Guide](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)
3. [OkHttp Bibliotek](https://square.github.io/okhttp/)
4. [Apache HttpClient](https://hc.apache.org/httpcomponents-client-5.0.x/)