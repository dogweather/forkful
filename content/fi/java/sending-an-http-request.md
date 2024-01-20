---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTTP-pyynnön lähettäminen on tapa, jonka avulla ohjelma voi hakea tai lähettää tietoja verkkosivuihin suoraan internetin kautta. Tämä on tärkeää, koska se mahdollistaa reaaliaikaisen datan jakamisen eri ohjelmien ja verkkosivujen välillä.

## Näin teet:

```Java
import java.net.http.*;
import java.net.URI;
import java.time.Duration;

public class Main {
  public static void main(String[] args) {
    HttpClient client = HttpClient.newHttpClient();
    HttpRequest request = HttpRequest.newBuilder()
      .uri(URI.create("http://example.com"))
      .timeout(Duration.ofMinutes(1))
      .header("Content-Type", "application/json")
      .GET()
      .build();
  
    HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
  
    System.out.println(response.statusCode());
    System.out.println(response.body());
  }
}
```

Koodiesimerkissämme luomme `HttpClient` -olion, määritämme HTTP-pyynnön `HttpRequest.newBuilder()` metodilla ja lähetämme pyynnön `client.send()` metodin avulla. Tulostamme lopulta vastauksen tilakoodin ja vartalon.

## Syvällisempää tietoa

HTTP-pyynnön lähettäminen on ollut olennainen osa web-sovellusten kehitystä jo vuosikymmenien ajan. Sen toteutus Java-ohjelmointikielessä on kuitenkin muuttunut vuosien varrella. Vaikka `HttpURLConnection` oli aiemmin standardi tapa lähettää HTTP-pyynnöt, Java 11 esitteli uuden HTTP Client API:n, jota käytimme esimerkkikoodissamme.

Vaihtoehtoja ovat kolmannen osapuolen kirjastot, kuten OkHttp ja Apache HttpClient, jotka tarjoavat lisäominaisuuksia, kuten multipart body tuen sekä paremman suorituskyvyn.

On tärkeää huomioida, että Java HTTP Client toimii sekä synkronisessa että asynkronisessa tilassa. Edellisessä esimerkissämme olemme käyttäneet synkronista tapaa käyttäen `client.send()` -metodia, mutta Java HTTP Client tarjoaa myös asynkronisen vaihtoehdon `client.sendAsync()` -metodin kautta.

## Katso myös

1. Java 11 HttpClient dokumentaatio: [linkki](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
3. OkHttp kirjasto: [linkki](https://square.github.io/okhttp/)
4. Apache HttpClient kirjasto: [linkki](https://hc.apache.org/httpcomponents-client-4.5.x/index.html)