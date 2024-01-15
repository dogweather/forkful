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

## Hvorfor
HTTP-forespørsler er en viktig del av å kommunisere med servere på internett og er avgjørende for å hente og sende data. Å kunne sende en HTTP-forespørsel riktig er essensielt for å bygge pålitelige og effektive nettapplikasjoner.

## Hvordan
Å sende en HTTP-forespørsel i C er enkelt. Først må du inkludere <stdio.h> og <curl/curl.h> biblioteker. Deretter kan du bruke følgende kode for å sende en HTTP GET-forespørsel til en URL:

```C
CURL *curl;
CURLcode res;
curl = curl_easy_init();
if(curl) {
  curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
  res = curl_easy_perform(curl);
  curl_easy_cleanup(curl);
}
```

Dette koden bruker libcurl-biblioteket for å sette opp en curl-forbindelse og sende en GET-forespørsel til "https://www.example.com". Hvis alt går bra, vil koden returnere et null-objekt, ellers vil det returnere en feilkode. Du kan legge til flere alternativer, som å legge til tilpassede HTTP-headere, ved å bruke curl_easy_setopt-funksjonen.

## Dykk dypere
Når du sender en HTTP-forespørsel, er det viktig å forstå de forskjellige komponentene og parametrene som er involvert. Det kan være nyttig å lese om HTTP-protokollen og dens ulike metoder, som GET, POST, PUT og DELETE. Du bør også være klar over HTTP-statuskoder, som gir informasjon om resultatet av forespørselen. Det er også mulig å sette tilpassede headere og parametre i HTTP-forespørselen for å spesifisere ytterligere informasjon.

## Se også
- [Libcurl dokumentasjon](https://curl.haxx.se/libcurl/)
- [HTTP-protokollen](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)
- [HTTP-statuskoder](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)