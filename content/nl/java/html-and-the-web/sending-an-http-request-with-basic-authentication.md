---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:10.929078-07:00
description: "Een HTTP-verzoek versturen met basisauthenticatie houdt in dat er een\
  \ header met een gebruikersnaam en wachtwoord wordt toegevoegd om toegang te krijgen\u2026"
lastmod: '2024-03-11T00:14:24.501943-06:00'
model: gpt-4-0125-preview
summary: "Een HTTP-verzoek versturen met basisauthenticatie houdt in dat er een header\
  \ met een gebruikersnaam en wachtwoord wordt toegevoegd om toegang te krijgen\u2026"
title: Een HTTP-verzoek verzenden met basisauthenticatie
---

{{< edit_this_page >}}

## Wat & Waarom?
Een HTTP-verzoek versturen met basisauthenticatie houdt in dat er een header met een gebruikersnaam en wachtwoord wordt toegevoegd om toegang te krijgen tot een beschermd bron. Programmeurs gebruiken dit voor eenvoudige autorisatie in webdiensten wanneer meer geavanceerde methoden niet nodig zijn.

## Hoe te:
Java maakt het vrij eenvoudig om HTTP-verzoeken met basisauthenticatie te sturen met behulp van de `HttpURLConnection` klasse. Hier is een snel voorbeeld:

```java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

public class BasicAuthRequest {

    public static void main(String[] args) {
        try {
            URL url = new URL("http://example.com/resource");
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            
            String gebruikersgegevens = "gebruiker:wachtwoord";
            String basicAuth = "Basic " + new String(Base64.getEncoder().encode(gebruikersgegevens.getBytes(StandardCharsets.UTF_8)));
            connection.setRequestProperty("Authorization", basicAuth);

            int responsecode = connection.getResponseCode();
            System.out.println("Reactiecode: " + responsecode);

            if (responsecode == HttpURLConnection.HTTP_OK) {
                BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
                String invoerregel;
                StringBuilder response = new StringBuilder();

                while ((invoerregel = in.readLine()) != null) {
                    response.append(invoerregel);
                }
                in.close();

                System.out.println(response.toString());
            } else {
                System.out.println("GET-verzoek werkte niet");
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
Voorbeelduitvoer:

```
Reactiecode: 200
{ "bericht": "Dit is een antwoord van een beveiligde bron!" }
```

## Diepere Duik
Basisauthenticatie bestaat al sinds de vroege dagen van HTTP. Het werkt door basis64-gecodeerde referenties in de header te zenden, waardoor het eenvoudig maar niet erg veilig is zonder HTTPS, aangezien referenties gemakkelijk gedecodeerd kunnen worden.

Alternatieven zoals OAuth voegen een extra beveiligingslaag toe door tokens te gebruiken in plaats van directe referenties. Token-gebaseerde authenticatie krijgt tegenwoordig de voorkeur, met name voor RESTful API's.

Bij het implementeren van basis toegangsauthenticatie in Java, is de aanbevolen methode sinds Java 11 het gebruiken van de nieuwe `HttpClient` klasse. Het is veelzijdiger en ondersteunt standaard HTTP/2. Echter, voor basisvereisten of verouderde systemen, blijft `HttpURLConnection` een haalbare optie.

## Zie Ook
- [RFC 7617 - Het basis-HTTP-authenticatieschema](https://tools.ietf.org/html/rfc7617)
- [Oracle Java 11 HTTP Client API Documentatie](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [Baeldung-gids over Java HTTP-verzoeken](https://www.baeldung.com/java-http-request)
