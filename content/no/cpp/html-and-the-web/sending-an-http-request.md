---
date: 2024-01-20 17:59:16.596029-07:00
description: "Hvordan gj\xF8re det: C++ har ingen innebygd HTTP-st\xF8tte, s\xE5 vi\
  \ m\xE5 bruke et bibliotek. `Curl` er et popul\xE6rt valg. Her er en grunnleggende\
  \ kode for \xE5 sende\u2026"
lastmod: '2024-03-13T22:44:41.096375-06:00'
model: gpt-4-1106-preview
summary: "C++ har ingen innebygd HTTP-st\xF8tte, s\xE5 vi m\xE5 bruke et bibliotek."
title: "\xC5 sende en HTTP-foresp\xF8rsel"
weight: 44
---

## Hvordan gjøre det:
C++ har ingen innebygd HTTP-støtte, så vi må bruke et bibliotek. `Curl` er et populært valg. Her er en grunnleggende kode for å sende en GET-forespørsel:

```C++
#include <iostream>
#include <curl/curl.h>

static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL *curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);

        res = curl_easy_perform(curl);
        if(CURLE_OK == res) {
            std::cout << "Fetched data:\n" << readBuffer << std::endl;
        }
        
        curl_easy_cleanup(curl);
    }

    return 0;
}
```

Ved kjøring vil dette skrive ut innholdet fra `http://example.com`.

## Dypdykk
HTTP-kommunikasjon er fundamentalt for webprogrammering. Før `libcurl` ble populært på 2000-tallet, var alternativene begrenset; programmerere skrev ofte sin egen nettverkskode eller brukte spesifikke biblioteker med varierende grad av kompleksitet og støtte.

Alternativene til `curl` inkluderer `Boost.Beast`, `cpprestsdk`, og lavnivå-tilnærminger som `sockets`. Når du velger et bibliotek, vurder støtten for asynkronitet, feilhåndtering, og kompleksiteten av HTTP-operasjoner du trenger.

## Se også
- [cURL offisielle nettside](https://curl.se/)
- [C++ Requests: Curl for People, kompakt HTTP-klient](https://github.com/whoshuu/cpr)
- [cpprestsdk GitHub repository](https://github.com/Microsoft/cpprestsdk)
- [Boost.Beast offisiell dokumentasjon](https://www.boost.org/doc/libs/release/libs/beast/)
