---
title:                "Å sende en HTTP-forespørsel"
aliases: - /no/cpp/sending-an-http-request.md
date:                  2024-01-20T17:59:16.596029-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å sende en HTTP-forespørsel"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel betyr å be en webserver om data eller handlinger. Programmerere gjør dette for å integrere nettressurser, hente informasjon, eller samhandle med andre tjenester.

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
