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

## Hvorfor

Å sende en HTTP forespørsel er en viktig del av webutvikling og kan hjelpe deg med å få tilgang til data og ressurser fra eksterne kilder. Det kan også brukes til å kommunisere med applikasjoner og APIer, noe som gjør det til et viktig verktøy for å bygge moderne og dynamiske nettsider.

## Slik gjør du det

For å sende en HTTP forespørsel i C++, må du først inkludere "curl/curl.h"-biblioteket i koden din. Dette biblioteket lar deg lage og utføre HTTP-forespørsler på en enkel måte.

Deretter kan du opprette en ny instans av CURL-objektet og angi URL-adressen til serveren du vil sende forespørselen til. Du kan også sette eventuelle parametere eller headers som kreves for å fullføre forespørselen.

```C++
#include <curl/curl.h>
// Opprett instans av CURL-objekt
CURL *curl = curl_easy_init();
// Angi URL-adresse
curl_easy_setopt(curl, CURLOPT_URL, "http://www.example.com");
```

Neste steg er å utføre selve forespørselen. Dette kan gjøres ved å kalle "curl_easy_perform" funksjonen og lagre resultatet i en variabel.

```C++
// Utfør forespørselen og lagre resultatet
CURLcode res = curl_easy_perform(curl);
// Kontroller for eventuelle feil
if(res != CURLE_OK) {
  fprintf(stderr, "curl_easy_perform() failed: %s\n",
          curl_easy_strerror(res));
}
```

For å behandle svaret fra serveren, kan du bruke "curl_easy_getinfo" funksjonen og angir ønsket informasjon som parameter. Dette kan omfatte statuskoder, svartider og innholdstype.

```C++
// Få statuskoden fra serveren
long statuscode;
curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &statuscode);
// Skriv ut statuskoden
printf("Statuskode: %ld\n", statuscode);
```

## Dypdykk

Når du sender en HTTP-forespørsel, er det flere elementer du bør være oppmerksom på. Dette inkluderer ulike typer forespørsler, som GET, POST, PUT og DELETE, samt hvordan du håndterer responsen fra serveren.

Du bør også sørge for å håndtere eventuelle feilsituasjoner, som når serveren ikke svarer eller når det oppstår en nettverksfeil. Dette kan gjøres ved å bruke returkoden fra "curl_easy_perform" funksjonen og sjekke for tilhørende feilmeldinger.

Selv om sending av en enkel HTTP-forespørsel kan virke som en enkel oppgave, er det viktig å følge beste praksis og sørge for å sikre at koden din håndterer alle potensielle utfall.

## Se også

- [C++ CURL-bibliotek dokumentasjon](https://curl.haxx.se/libcurl/c/)
- [En guide til HTTP-forespørsler i C++](https://www.theserverside.com/feature/A-guide-to-HTTP-requests-in-C)
- [Eksempler på bruk av CURL i C++ programmering](https://stackoverflow.com/questions/978061/http-post-request-in-c)