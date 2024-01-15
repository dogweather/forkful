---
title:                "Sending en http forespørsel med grunnleggende autentisering"
html_title:           "C: Sending en http forespørsel med grunnleggende autentisering"
simple_title:         "Sending en http forespørsel med grunnleggende autentisering"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor skulle noen ønske å sende en HTTP-forespørsel med grunnleggende autentisering? Vel, dette er ofte nødvendig når man skal få tilgang til beskyttede ressurser på en nettside eller et web-API.

## Hvordan
Den grunnleggende autentiseringsmekanismen innebærer å legge til en Base64-kodet brukernavn og passord i HTTP-headeren `Authorization`. Her er et eksempel i C:

```C
#include <stdio.h>
#include <curl/curl.h> // Krever cURL-biblioteket

int main(void) {
  CURL *curl = curl_easy_init();
  if(curl) {
    // Angir URL og setter HEADER til "Authorization: Basic base64
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
    struct curl_slist *headers = NULL;
    headers = curl_slist_append(headers, "Authorization: Basic dXNlcm5hbWU6cGFzc3dvcmQ=");
    curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);

    CURLcode res = curl_easy_perform(curl);
    if(res != CURLE_OK)
      fprintf(stderr, "cURL error: %s\n", curl_easy_strerror(res));

    curl_easy_cleanup(curl);
    curl_slist_free_all(headers);
  }
  return 0;
}
```

Dette eksempelet bruker cURL-biblioteket for å sende en HTTP-forespørsel til nettsiden `http://example.com` med en grunnleggende autentiseringsheader på følgende form: `Authorization: Basic [base64-encoded brukernavn:passord]`. Output vil variere avhengig av hvordan nettsiden behandler denne autentiseringsmetoden.

## Dypdykk
Grunnleggende autentisering er den enkleste formen for autentisering, men den har sine ulemper og bør ikke brukes alene for å sikre sensitiv informasjon. I stedet bør det brukes i kombinasjon med andre autentiseringsmetoder som f.eks. HTTPS og token-basert autentisering.

Det er også viktig å merke seg at brukernavn og passord er lagret i base64-kodet form, noe som ikke er en effektiv krypteringsmetode. Det er derfor viktig å bruke HTTPS for å sikre at denne informasjonen ikke kan fanges opp av uvedkommende.

## Se også
- [Grunnleggende autentisering (Wikipedia)](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [cURL Library API in C](https://curl.se/libcurl/c/)
- [HTTPS Explained (Video)](https://www.youtube.com/watch?v=T4Df5_cojAs)