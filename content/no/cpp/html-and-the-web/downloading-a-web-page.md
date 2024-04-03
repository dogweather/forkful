---
date: 2024-01-20 17:44:02.326346-07:00
description: "How to: For \xE5 laste ned en nettside i C++, kan du bruke biblioteket\
  \ `libcurl`. Her er et enkelt eksempel."
lastmod: '2024-03-13T22:44:41.098376-06:00'
model: gpt-4-1106-preview
summary: "For \xE5 laste ned en nettside i C++, kan du bruke biblioteket `libcurl`."
title: Nedlasting av en nettside
weight: 42
---

## How to:
For å laste ned en nettside i C++, kan du bruke biblioteket `libcurl`. Her er et enkelt eksempel:

```C++
#include <iostream>
#include <curl/curl.h>

static size_t write_callback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

void download_webpage(const std::string &url) {
    CURL *curl = curl_easy_init();
    if(curl) {
        std::string response_string;
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str()); // Setter URL
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback); // Callback for datamottak
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response_string); // Data for å skrive til
        
        CURLcode res = curl_easy_perform(curl); // Utfør request
        if(res != CURLE_OK) {
            std::cerr << "curl_easy_perform() failed: " << curl_easy_strerror(res) << std::endl;
        } else {
            std::cout << response_string << std::endl; // Printer HTML
        }
        curl_easy_cleanup(curl);
    }
}

int main() {
    const std::string url = "http://example.com";
    download_webpage(url);
    return 0;
}
```

Forventet output er HTML-innholdet til `example.com`.

## Deep Dive:
Før `libcurl`, måtte programmerere på Unix systemer gjerne stole på systemkall for å gjøre `HTTP`-requests via verktøy som `wget` eller `curl` i kommandolinjen. `libcurl` gir en programmeringsgrensesnitt (API) for flere språk, inkludert C++, noe som gjør det mulig å implementere nedlasting av websider på en mer bærbar og integrert måte.

There are alternatives, such as `Boost.Asio` for asynchronous network programming, or `Poco` libraries that offer more than just network capabilities. It all depends on the needs and constraints of your project.

Hoveddetaljer til å tenke på inkluderer håndtering av HTTP-protokoller, SSL/TLS for sikre forbindelser, og foutbehandling. C++ gir ingen innebygde nettverks biblioteker i standardbiblioteket (per Knowledge Cut-off, Januar 2023), så tredjepartsbiblioteker blir ofte brukt.

## See Also:
For further reading and resources, you may check the following:

- cURL's official website for documentation and guides: https://curl.haxx.se/
- CPP Reference for C++ standard library documentation: https://en.cppreference.com/
- Boost.Asio for asynchronous networking: https://www.boost.org/doc/libs/1_75_0/doc/html/boost_asio.html
- Poco Project for the Poco C++ Libraries: https://pocoproject.org/
