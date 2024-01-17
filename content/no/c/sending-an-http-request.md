---
title:                "Sende en http-forespørsel"
html_title:           "C: Sende en http-forespørsel"
simple_title:         "Sende en http-forespørsel"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?

Å sende en HTTP-forespørsel er en vanlig praksis blant programmerere for å kommunisere med servere og få tilgang til data og ressurser. Dette gjøres ved å sende en forespørsel til en spesifikk URL-adresse og motta en respons fra serveren. Dette er nyttig for å hente informasjon fra andre nettsteder, integrere tjenester og utføre ulike handlinger som krever serverkommunikasjon.

Slik gjør du:

```C
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h> // bibliotek for å sende HTTP-forespørsler

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init(); // initialiserer curl-objektet
  
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://en.wikipedia.org/wiki/Main_Page"); // angir URL-adressen å sende forespørselen til
    res = curl_easy_perform(curl); // utfører forespørselen og lagrer responsen i "res"-variabelen

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    curl_easy_cleanup(curl); // rydder opp etter utført forespørsel
  }
  return 0;
}
```

Dypdykk:

Å sende HTTP-forespørsler har vært en viktig del av nettverkskommunikasjon siden starten av World Wide Web. Alternativene til å bruke biblioteker som curl for å sende forespørsler er åpne kilder som gjør det mulig å sende forespørsler direkte gjennom HTTP-protokollen, eller å bruke innebygde funksjoner fra språk som Python eller JavaScript.

curl-biblioteket tilbyr også mange forskjellige konfigurasjonsmuligheter for å tilpasse forespørselene, som å legge til HTTP-hodere, håndtere HTTPS-sertifikater og begrense tidsutførelse.

Se også:

- curl bibliotekets hjemmeside: https://curl.se/libcurl/
- HTTP-protokollen: https://tools.ietf.org/html/rfc2616