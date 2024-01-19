---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel med grunnleggende autentisering er en akt en programmerer gjør for å tilgang til eller hente data fra sikre servere. Programmerere gjør dette for å sikre at bare autoriserte brukere har tilgang til beskyttede ressurser.

## Hvordan Gjøre Det:
For å sende en HTTP forespørsel med Basic Authentication, kan vi bruke `libcurl`, et gratis og lett-å-bruke klient-side URL transfer bibliotek. Ditt C program skal inkludere biblioteket `#include <curl/curl.h>` for å benytte dette.

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);
  curl = curl_easy_init();
  
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://api.website.com/data");

    /* Brukernavn og passord for Basic Authentication */
    curl_easy_setopt(curl, CURLOPT_USERPWD, "brukernavn:passord");

    /* Utføre forespørsel, res vil få returkoden */
    res = curl_easy_perform(curl);

    /* Sjekke for feil */ 
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }
  
  curl_global_cleanup();

  return 0;
}
```
Når dette programmet kjøres, sender det en HTTP-forespørsel til api.website.com med Basic Authentication brukernavn og passord.

## Dyp Dykk
Historisk sett, Basic Authentication ble introduksert som en del av HTTP/1.0-spesifikasjonen. Det tilbyr en enkel måte å sikre brukergodkjenning over HTTP-protokollen. 

I moderne programmering er det alternativer til Basic Authentication, slik som OAuth2 og JWT (JSON Web Token).

I forhold til implementasjon, skal du ta hensyn til at Basic Authentication sender brukernavn og passord i klartekst (Base64-kodet, ikke kryptert). Derfor skal det brukes sammen med HTTPS for å unngå at sensitiv informasjon blir hacket.

## Se Også 
1. [Mozilla Developer Network - HTTP autentisering](https://developer.mozilla.org/nb/docs/Web/HTTP/Authentication)
2. [libcurl - Tutorial](https://curl.se/libcurl/c/libcurl-tutorial.html)
3. [JWT.io - Introduksjon til JSON Web Tokens](https://jwt.io/introduction/)
4. [OAuth (offisielle side)](https://oauth.net/)