---
title:                "Å sende en HTTP-forespørsel"
date:                  2024-01-20T17:59:04.057114-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å sende en HTTP-forespørsel"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Å sende en HTTP-forespørsel innebærer å be en webserver om data eller utføre en handling. Programmerere gjør dette for å integrere nettjenester, hente informasjon, eller interagere med APIer.

## How to: (Slik gjør du:)
For å sende en HTTP-forespørsel i C, kan du bruke `libcurl`, et kraftig bibliotek for overføringer av URLer. Følgende er et enkelt eksempel på hvordan du bruker `libcurl` til å gjøre en GET-forespørsel:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);

        res = curl_easy_perform(curl);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));

        curl_easy_cleanup(curl);
    }
    return 0;
}
```
Når du kjører koden, vil du se innholdet av `http://example.com` i konsollen din.

## Deep Dive (Dypdykk)
HTTP-forespørsler har vært en sentral del av webprogrammering siden 90-tallet. `libcurl` tilbyr ikke bare GET-forespørsler, men også POST, PUT og mange andre HTTP-metoder. Alternativer til `libcurl` inkluderer `libhttp` eller lavnivå sockets, men `libcurl` er bredt akseptert for sin enkelhet og robusthet.

Å bygge en HTTP-forespørsel krever en viss struktur: en startlinje, hoder (headers), og noen ganger en meldingskropp. `libcurl` håndterer disse detaljene for deg, men det er nyttig å forstå HTTP-protokollen for å skreddersy forespørsler effektivt.

## See Also (Se også)
- curl website: [https://curl.se/](https://curl.se/)
- Wikipedia on HTTP: [https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)
- 'libcurl' dokumentasjon: [https://curl.se/libcurl/](https://curl.se/libcurl/)
