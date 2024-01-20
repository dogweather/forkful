---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel er en prosess der en klient (vanligvis en nettleser) sender en forespørsel til serveren for å få data. Dette er kritisk for programvareutviklere fordi det lar oss hente data fra en webserver, oppdatere data på serveren, eller til og med slette noen data fra serveren. 

## Hvordan Gjøre:

HTTP-forespørsler kan sendes i C++ ved hjelp av biblioteker som libcurl eller Boost. Her er et eksempel på hvordan du sender en GET forespørsel med libcurl.

```C++
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://www.example.com");

    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }
  return 0;
}
```

Dette programmet vil hente innholdet på www.example.com og skrive det til standard ut. Hvis det oppstår en feil, vil du få en feilmelding.

## Dyp Dykk

HTTP-forespørsler har vært en del av webprogrammering siden begynnelsen av internett. Først ble det gjort i kommandolinjen, men så utviklet vi biblioteker og rammeverk som forenklet prosessen. 

Alternativt til libcurl, kan man også bruke Boost, Poco, og andre lignende C++-biblioteker for å håndtere HTTP-forespørsler. Valget avhenger av prosjektkravene og personlige preferanser.

Når du sender en HTTP-forespørsel, skjer det flere viktige ting under panseret. For eksempel håndterer biblioteket DNS-oppslag, TCP-tilkobling, SSL-håndtrykk (for HTTPS), og til slutt HTTP-forespørselen selv.

## Se Også

1. [libcurl C API](https://curl.haxx.se/libcurl/c/)
2. [Boost library](https://www.boost.org/doc/libs/1_61_0/doc/html/boost_asio.html)
3. [Poco C++ libraries](https://pocoproject.org/)