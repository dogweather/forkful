---
title:                "Å sende en http-forespørsel med grunnleggende autentisering"
html_title:           "C: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Når du sender en HTTP forespørsel med en grunnleggende autentisering, inkluderer du brukernavnet og passordet ditt i forespørselen for å bekrefte identiteten din. Dette gjør at serveren du sender forespørselen til kan validere at du har autorisasjon til å få tilgang til de etterspurte ressursene. Programmere gjør dette for å sikre at bare autoriserte brukere kan få tilgang til bestemte deler av en nettside eller applikasjon.

## Slik gjør du det:
```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
  CURL *curl;
  CURLcode res;
 
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
    curl_easy_setopt(curl, CURLOPT_USERNAME, "username");
    curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");
 
    res = curl_easy_perform(curl);
 
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
 
    curl_easy_cleanup(curl);
  }
  return 0;
}
```
Kodeeksempelet viser hvordan du kan sende en HTTP forespørsel med grunnleggende autentisering ved hjelp av libcurl biblioteket. Etter å ha initialisert og satt opp en CURL handle, kan du angi ønsket URL og legge til autentiseringsopplysningene dine ved å bruke `CURLOPT_HTTPAUTH`, `CURLOPT_USERNAME` og `CURLOPT_PASSWORD` innstillinger. Deretter kan du utføre forespørselen ved hjelp av `curl_easy_perform()` funksjonen.

## Dypdykk:
HTTP forespørsler med grunnleggende autentisering var en vanlig metode for å sikre identiteten til brukere og tillate tilgang til beskyttede ressurser. Men på grunn av sikkerhetsrisikoene som følger med å sende passord i klartekst, har andre autentiseringsmetoder som token-autentisering blitt mer vanlig.

Implementeringen av HTTP forespørselen med grunnleggende autentisering kan variere avhengig av hvilket bibliotek eller API du bruker. Det er også viktig å sørge for at nettstedet eller applikasjonen du sender forespørselen til støtter grunnleggende autentisering og at passordet ditt er sikkert lagret.

## Se også:
- [libcurl dokumentasjon](https://curl.haxx.se/libcurl/c/http-auth.html)
- [Tutorial: Enkel autentisering med cURL](https://www.codetd.com/en/article/36053474)