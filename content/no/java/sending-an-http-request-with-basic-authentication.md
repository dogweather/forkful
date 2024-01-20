---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel med grunnleggende autentisering betyr at vi sender en forespørsel til en server, med brukernavn og passord inkludert. Programmerere gjør dette for å sikre tilgang til beskyttet innhold på websider.

## Hvordan gjøre det:

Java 11 introduserte den innebygde HTTP-klienten som gjør HTTP-forespørsler mye friksjonsfrie. Her er et eksempel:

```Java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpHeaders;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.Base64;

public class Main {
  public static void main(String[] args) throws Exception {
    String name = "brukernavn";
    String password = "passord";
    String auth = name + ":" + password;

    String encodedAuth = Base64.getEncoder().encodeToString(auth.getBytes());
    
    HttpClient client = HttpClient.newHttpClient();    
    HttpRequest request = HttpRequest.newBuilder()
        .uri(new URI("http://example.com"))
        .header("Authorization", "Basic " + encodedAuth)
        .build();
        
    HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
    
    System.out.println(response.body());
  }
}
```

## Dyp Dykk

*Historisk sammenheng*: Basic authentication har vært standard metode for autentisering siden tidlige dager av weben, selv om det har begrensninger, som at brukerens legitimasjon sendes uten kryptering.

*Alternativer*: Av sikkerhetsgrunner, har mange alternativer til grunnleggende autentisering blitt utviklet, som Digest Access Authentication, OAuth, og mer kompliserte autentiseringsskjemaer med SSL og JWT.

*Gjennomføringsdetaljer*: I eksempelet over, bruker vi `Base64.getEncoder().encodeToString()` for å sende brukernavn og passord kodet i Base64 i HTTP-headeren. 

## Se også

* [Java HttpClient dokumentasjon](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
* [Java Base64 Encoder dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/util/Base64.Encoder.html)
* [Tutorial om Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication) på Mozilla Developer Network