---
title:                "C: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor
HTTP-forespørsler er en viktig del av dagens nettverkstrafikk og er avgjørende for å få til korte, effektive og brukervennlige applikasjoner. Ved å sende en HTTP-forespørsel kan du få tilgang til ressurser på en server fra en annen kilde, og dette er grunnleggende for å få tilgang til data og tjenester på nettet.

## Hvordan
For å sende en HTTP-forespørsel i C, er det flere muligheter å velge mellom. En måte er å bruke et bibliotek som cURL, som gir en enkel og intuitiv måte å lage og sende HTTP-forespørsler på. Her er et eksempel på hvordan du kan bruke cURL til å hente inn en nettside og skrive ut svaret i terminalen:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
    CURL *curl;
    CURLcode res;
    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
        res = curl_easy_perform(curl);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Når du kjører dette eksempelet, vil du få utskrevet HTML-koden fra nettsiden i terminalen. Dette viser hvordan man kan bruke cURL for å lage en HTTP-forespørsel og få tilgang til informasjon fra en ekstern kilde.

## Dypdykk
I tillegg til å bruke et bibliotek som cURL, kan man også lage HTTP-forespørsler ved å bruke TCP-sockets direkte i C. Dette gir mer kontroll og mulighet for å tilpasse detaljer i requestet. Et eksempel på hvordan dette kan gjøres er beskrevet i [dette blogginnlegget](https://aticleworld.com/http-get-and-post-methods-example-in-c/) på Aticleworld.

For å forstå hvordan HTTP-forespørsler fungerer i detalj, er det også nyttig å lese om HTTP-protokollen. En god kilde for dette er [dette innlegget](https://daniel.haxx.se/blog/2011/03/10/http-1-1-and-other-stray-dogs/) av Daniel Stenberg.

## Se også
- [cURL-dokumentasjon](https://curl.se/libcurl/c/)
- [TCP Socket-programmering i C](https://www.geeksforgeeks.org/socket-programming-cc/)
- [HTTP-protokollspesifikasjon](https://tools.ietf.org/html/rfc7230)