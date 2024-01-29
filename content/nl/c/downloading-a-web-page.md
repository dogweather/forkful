---
title:                "Een webpagina downloaden"
date:                  2024-01-28T21:59:25.726938-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een webpagina downloaden"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een webpagina downloaden betekent dat je de HTML-inhoud ervan ophaalt van de webserver waarop deze zich bevindt. Programmeurs doen dit om de gegevens van de webpagina offline te verwerken, analyseren of ermee te interageren.

## Hoe:
```C
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t geschreven = fwrite(ptr, size, nmemb, stream);
    return geschreven;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char uitbestandsnaam[FILENAME_MAX] = "downloaded_page.html";

    curl = curl_easy_init();
    if (curl) {
        fp = fopen(uitbestandsnaam,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        
        res = curl_easy_perform(curl);
        /* Controleer op fouten */
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() is mislukt: %s\n",
                curl_easy_strerror(res));
        
        /* Opruimen */
        curl_easy_cleanup(curl);
        fclose(fp);
    }

    return 0;
}
```
Voorbeelduitvoer:
```
(Geen uitvoer, maar controleer de huidige map op een 'downloaded_page.html' bestand)
```

## Diepgaande duik
Terug in de vroege dagen van het internet, het grijpen van een webpagina betrokken rauwe HTTP-verzoeken via TCP sockets - omslachtig, om het zacht te zeggen. Tegenwoordig hebben we bibliotheken zoals libcurl, die het zware werk uit het proces halen. Het handelt alle lastige onderdelen van HTTP-verzoeken, SSL-verbindingen en meer af.

Er zijn een paar alternatieven voor libcurl zoals wget en http-client in C, maar libcurl wordt veel gebruikt vanwege zijn robuustheid en functies. Wanneer je libcurl gebruikt, houd dan het volgende in gedachten:

- Initialisatie met `curl_easy_init()` is een must.
- Stel opties in die geschikt zijn voor je behoeften; voor het downloaden moeten we de URL en de schrijffunctie specificeren.
- `CURLOPT_WRITEFUNCTION` stelt ons in staat een pointer naar onze callback-functie door te geven om de gegevens naar een bestand te schrijven.
- Controleer altijd het resultaat van `curl_easy_perform()` op fouten.
- Vergeet niet op te ruimen met `curl_easy_cleanup()` om lekken te voorkomen.

Voor productiecode zou je foutafhandeling willen, HTTP-statuscodes controleren en beveiligingsoverwegingen beheren (zoals SSL-certificaatverificatie).

## Zie ook
- [libcurl](https://curl.se/libcurl/)
- [HTTP Made Really Easy](https://www.jmarshall.com/easy/http/)
