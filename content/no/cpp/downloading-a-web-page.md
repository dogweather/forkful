---
title:                "C++: Last ned en nettside"
simple_title:         "Last ned en nettside"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Å laste ned en nettside kan være nyttig for å få tilgang til informasjon fra en nettside uten å måtte være koblet til internett. Dette kan være spesielt nyttig når man er uten nettforbindelse eller for å gjøre store mengder informasjon lettere å organisere og behandle.

## Hvordan gjøre det

For å laste ned en nettside i en C++ applikasjon, kan man bruke biblioteket "libcurl". Dette biblioteket har funksjoner for å hente data fra en nettside og lagre det i en variabel. Etter at man har inkludert biblioteket i koden, kan man bruke funksjonen "curl_easy_init()" for å opprette en "curl" -håndterer. Deretter kan man bruke funksjonen "curl_easy_setopt()" for å konfigurere håndtereren til å hente data fra en spesifikk URL. Til slutt kan man bruke funksjonen "curl_easy_perform()" for å faktisk hente data fra nettsiden og lagre det i en buffer.

Et eksempel på kode som henter data fra Google sin hjemmeside og lagrer det i en variabel:

```C++
#include <iostream>
#include <curl/curl.h>

// Denne funksjonen blir brukt av curl for å lagre data som blir lastet ned
static size_t writeCallback(void *contents, size_t size, size_t nmemb, void *userp){
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main(){
    // Opprett en curl håndterer
    CURL *curl_handle;
    curl_handle = curl_easy_init();

    // Konfigurer håndtereren til å hente data fra Google sin hjemmeside
    curl_easy_setopt(curl_handle, CURLOPT_URL, "https://www.google.com/");

    // Opprett en variabel for å lagre dataene som blir lastet ned
    std::string buffer;

    // Fortell curl å bruke writeCallback funksjonen for å lagre data i variabelen
    curl_easy_setopt(curl_handle, CURLOPT_WRITEFUNCTION, writeCallback);
    curl_easy_setopt(curl_handle, CURLOPT_WRITEDATA, &buffer);

    // Utfør handlingen og hent data fra nettsiden
    CURLcode res = curl_easy_perform(curl_handle);

    // Sjekk om handlingen var vellykket
    if(res != CURLE_OK)
        std::cout << "Error: " << curl_easy_strerror(res) << std::endl;
    else{
        // Dataene er nå lagret i variabelen "buffer"
        std::cout << buffer << std::endl;
    }

    // Frigjør curl håndtereren
    curl_easy_cleanup(curl_handle);

    return 0;
}
```

Eksempel på output fra koden over:

```
<!doctype html>
<html>
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width,initial-scale=1,minimum-scale=1,maximum-scale=1,user-scalable=no">
    <title>Google</title>
    <script nonce="AMMFawoaxNU8hMSnyFfF7g==">;(function(){
        window.google={kEI:'IykmW4j8GMTPkwXrt6KQBg',kEXPI:'201436,13533,205636,215283,219930,222014,222254,222332,222440,224347,226483,238830,243369,243396,244497,245230,245345,245557,1489930,1498414,1498646,1500014,1500889,1502615,1502914,1503141,1503284,1504149,1504247,7