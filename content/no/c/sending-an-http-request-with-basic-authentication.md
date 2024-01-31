---
title:                "Å sende en HTTP-forespørsel med grunnleggende autentisering"
date:                  2024-01-20T18:00:52.176852-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å sende en HTTP-forespørsel med grunnleggende autentisering"

category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel med basisgodkjenning betyr at man legger ved brukernavn og passord i en forespørsel for å identifisere seg. Programmerere gjør dette for å få tilgang til beskyttede ressurser på nettet.

## Hvordan:
```C
#include <stdio.h>
#include <curl/curl.h>

int main() {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();
    if(curl) {
        // Setter URL for HTTP-forespørsel og bruker basic authentication
        curl_easy_setopt(curl, CURLOPT_URL, "http://eksempel.no/data");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERPWD, "bruker:passord");

        // Utfører HTTP-forespørselen
        res = curl_easy_perform(curl);

        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() feilet: %s\n", curl_easy_strerror(res));
        
        // Rydder opp
        curl_easy_cleanup(curl);
    }
    curl_global_cleanup();
    return 0;
}
```
Sample output (på terminalen):
```
curl_easy_perform() feilet: Failed to connect to eksempel.no port 80: Connection refused
```
Merk: Den ovenforstående output indikerer et problem med forbindelsen, ikke koden.

## Deep Dive
Basisgodkjenning (basic authentication) har vært en enkel autentiseringsmekanisme siden det tidlige webben. I dag, sikrere alternativer som OAuth er oftere anbefalt. Når du implementerer basisgodkjenning, konvertér brukernavn og passord til en Base-64-kodet streng som inkluderes i HTTP-headeren. cURL-biblioteket i C gjør dette håndteringen enkelt, men det er viktig å alltid sende sensitiv informasjon over HTTPS for å forhindre intercepting.

## Se Også
- cURL bibliotekets offisielle dokumentasjon: https://curl.se/libcurl/c/
- Basic authentication over HTTPS: https://tools.ietf.org/html/rfc7617
- Sikrere autentiseringsalternativer: https://oauth.net/
