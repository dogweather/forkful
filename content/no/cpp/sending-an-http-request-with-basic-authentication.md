---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Å sende HTTP forespørsel med grunnleggende autentisering i C++  

## Hva & Hvorfor?   
Å sende en HTTP forespørsel med grunnleggende autentisering er en måte for et program å få tilgang til ressurser over nettet – spesielt de nettadressene som krever en brukernavn og passord. Programmerere gjør dette for å hente og poste data på sikret webområder på vegne av brukeren.  

## Hvordan:   
Her er et grunnleggende eksempel på hvordan du kan sende en GET-forespørsel med grunnleggende autentisering i C++, ved bruk av libcurl-biblioteket. 

```C++
#include <curl/curl.h>
#include <string>

void sendHttpRequest() {
    const char* url = "https://example.com";
    const char* username = "user";
    const char* password = "password";

    CURL* curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, username);
        curl_easy_setopt(curl, CURLOPT_PASSWORD, password);

        CURLcode res = curl_easy_perform(curl); 
        if(res != CURLE_OK)
          fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

        curl_easy_cleanup(curl);
    }
}
```
Bemerk at du må lenke mot libcurl i byggeprosessen for å bruke ovenstående kode.

## Dyp Dykk   
Grunnleggende HTTP-autentisering fikk sin oppstart i de tidlige dagene av webben som en enkel, usofistikert måte å hente ressurser fra sikret nettsteder. Men vær oppmerksom på at brukernavnet og passordet som sendes over nettet er i klartekst. Over tid, har det blitt mange nye teknikker som oAuth og tokenbasert autentisering som gir mer sikkerhet og fleksibilitet.  

Hvis libcurl ikke er et valg for prosjektet ditt, kan du også se på andre biblioteker som CppREST (tidligere kjent som Casablanca) eller POCO HTTPClient for å utføre HTTP-forespørsler med grunnleggende autentisering.

På implementasjonsnivå kan det være noen forskjeller i håndteringen av HTTP-autentisering avhengig av operativsystemet og biblioteket du bruker. Derfor er det viktig å attferdigstille seg med dokumentasjonen før du implementerer disse detaljene.

## Se også   
Curl dokumentasjon: https://curl.haxx.se/libcurl/c/   
CppREST SDK GitHub: https://github.com/microsoft/cpprestsdk  
POCO dokumentasjon: https://pocoproject.org/docs/ Poco.Net.HTTPClientSession.html