---
title:                "C: Nedlasting av en nettside"
simple_title:         "Nedlasting av en nettside"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Å laste ned en webside ved hjelp av C-programmering kan være nyttig for å hente eller manipulere data fra internett. Dette kan være nyttig for å hente informasjon fra nettsider for å bruke i andre programmer eller for å automatisere oppgaver som ellers ville blitt gjort manuelt.

## Hvordan

Vi skal se på et enkelt eksempel på hvordan man kan laste ned en webside ved hjelp av C-programmering. Først må vi inkludere `<stdio.h>` og `<curl/curl.h>` bibliotekene.

```C
#include <stdio.h>
#include <curl/curl.h>
```

Deretter må vi opprette en funksjon for å håndtere dataene som vi henter fra websiden. Dette kan gjøres ved hjelp av `CURL_WRITEFUNCTION` og `fwrite()` funksjonen.

```C
size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
   return fwrite(ptr, size, nmemb, stream);
}
```

Nå kan vi opprette en `CURL`-håndterer og sette innstillingene våre. Vi må spesifisere URL-en til websiden vi ønsker å laste ned og også sette `CURLOPT_WRITEFUNCTION` til å bruke funksjonen vi opprettet tidligere.

```C
CURL *curl;
CURLEcode res;

FILE *fp;

char *url = "https://www.eksempel.no/";
char outfilename[FILENAME_MAX] = "webside.html";

curl = curl_easy_init();

if (curl) {
    fp = fopen(outfilename, "wb");
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
    res = curl_easy_perform(curl);

    /* Sjekk for feil */
    if(res != CURLE_OK)
        fprintf(stderr, "laste ned feilet: %s\n",
                curl_easy_strerror(res));
    
    /* Lukk filen og frigjør ressurser */
    curl_easy_cleanup(curl);
}
```

Når programmet vårt kjører, vil websiden bli lastet ned og lagret som en HTML-fil med navnet "webside.html". Dette er et enkelt eksempel, men denne metodikken kan utvides og tilpasses for å hente mer data eller for å håndtere andre typer nettverkskall.

## Dypdykk

Det å laste ned en webside ved hjelp av C-programmering kan også innebære å bruke spesifikke protokoller som HTTP eller HTTPS, samt å sende forespørsler og motta svar fra serveren. Det er også viktig å håndtere eventuelle feil som kan oppstå underveis, som for eksempel manglende tilgang til en nettside eller en ugyldig URL. Det finnes også forskjellige biblioteker og rammeverk som kan hjelpe med å gjøre prosessen med å laste ned og håndtere data fra websider enda enklere.

## Se også

- [Libcurl Library](https://curl.se/libcurl/)
- [Open Source C Programming Resources](https://www.cprogramming.com/)
- [The Ultimate Resource for C Programming](https://www.learn-c.org/)