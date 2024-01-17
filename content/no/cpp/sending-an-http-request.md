---
title:                "Sende en http-forespørsel"
html_title:           "C++: Sende en http-forespørsel"
simple_title:         "Sende en http-forespørsel"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

Hva & Hvorfor? 

Sending av HTTP-forespørsler er en viktig del av nettverksprogrammering. Det tillater utveksling av data mellom en klient og en server gjennom Internett. Programmører bruker HTTP-forespørsler for å få tilgang til og hente informasjon fra forskjellige nettsider og API-er.

Hvordan gjør man det:

```C++
// Eksempel på hvordan man sender en HTTP-forespørsel i C++

#include <iostream>
#include <cstdlib>
#include <curl/curl.h>

int main() {
    CURL *curl;
    CURLcode res;

    // Oppretter og initialiserer curl
    curl = curl_easy_init();

    if(curl) {
        // Setter URL-adressen for å hente informasjon
        curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/");

        // Henter informasjonen ved å utføre GET-forespørsel
        res = curl_easy_perform(curl);

        // Sjekker om forespørselen var vellykket
        if(res == CURLE_OK) {
            std::cout << "Forespørselen ble gjennomført uten feil!" << std::endl;
        } else {
            std::cout << "Noe gikk galt under sending av forespørselen." << std::endl;
        }

        // Rydder opp i curl
        curl_easy_cleanup(curl);
    }
    
    return 0;
}
```

```
Output:
Forespørselen ble gjennomført uten feil!
```

Dypdykk:

HTTP (HyperText Transfer Protocol) ble utviklet på 1990-tallet og er et protokoll for å sende og motta informasjon over Internett. I dag er det en av de mest brukte protokollene for nettverkskommunikasjon og brukes også som grunnlag for andre protokoller som HTTPS og FTP. Det finnes også alternative måter å sende forespørsler på, som for eksempel å bruke biblioteker som libcurl.

Se også:

- [CURL Documentation](https://curl.haxx.se/libcurl/)
- [HTTP - Wikipedia](https://no.wikipedia.org/wiki/HTTP)
- [HTTP vs HTTPS - What's the Difference?](https://www.cloudflare.com/learning/ssl/http-vs-https/)