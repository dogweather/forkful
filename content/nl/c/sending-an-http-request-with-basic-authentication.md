---
title:                "Een HTTP-verzoek verzenden met basisauthenticatie"
date:                  2024-01-28T22:07:55.785106-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden met basisauthenticatie"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het versturen van een HTTP-verzoek met basisauthenticatie houdt in dat er een header wordt toegevoegd met een gebruikersnaam en wachtwoord om toegang te krijgen tot beschermde bronnen. Programmeurs doen dit om te interageren met webservices die inloggegevens vereisen voor de werking.

## Hoe te:
Om een HTTP-verzoek met basisauthenticatie in C te versturen, gebruik je meestal een bibliotheek zoals libcurl. Hier is een kort voorbeeld:

```c
#include <stdio.h>
#include <curl/curl.h>

int main() {
    CURL *curl = curl_easy_init();
    if(curl) {
        // Stel de URL in die ons POST-verzoek gaat ontvangen
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        
        // Stel de basisauthenticatie inloggegevens in
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERPWD, "gebruikersnaam:wachtwoord");
        
        // Voer het verzoek uit
        CURLcode res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() is mislukt: %s\n", curl_easy_strerror(res));
        }
        
        // Schoonmaken
        curl_easy_cleanup(curl);
    }
    
    return 0;
}
```

De uitvoer is afhankelijk van de reactie van de server.

## Diepgaande Duik
Een HTTP-verzoek versturen met basisauthenticatie is een vrij oude methode om toegang te beheren tot webbronnen. Ontworpen in de beginjaren van het internet, is het niet de meest veilige methode omdat de inloggegevens met base64 worden gecodeerd, niet versleuteld.

Alternatieven zoals OAuth en API-sleutels worden nu aanbevolen voor betere beveiliging. Echter, basisauthenticatie is nog steeds nuttig voor eenvoudige scripts of interne tools waarbij deze risico's acceptabel zijn.

Implementatie wordt meestal gedaan met bibliotheken zoals libcurl of aangepaste socketprogrammering als je meer controle nodig hebt. Basisauthenticatie headers kunnen handmatig worden geconstrueerd, maar dit is omslachtig en foutgevoelig, dus bibliotheken zijn de weg te gaan.

## Zie Ook
- cURL-bibliotheek documentatie: https://curl.haxx.se/libcurl/c/
- RFC 7617, Het 'Basic' HTTP-authenticatieschema: https://tools.ietf.org/html/rfc7617
- HTTP-authenticatie MDN-webdocumenten: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Introductie tot OAuth: https://oauth.net/
