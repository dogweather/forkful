---
date: 2024-01-20 17:59:34.488873-07:00
description: "Att skicka en HTTP-beg\xE4ran inneb\xE4r att din applikation kommunicerar\
  \ med en webbserver, vanligtvis f\xF6r att h\xE4mta eller skicka data. Programmerare\
  \ g\xF6r det\u2026"
lastmod: '2024-03-13T22:44:38.205966-06:00'
model: gpt-4-1106-preview
summary: "Att skicka en HTTP-beg\xE4ran inneb\xE4r att din applikation kommunicerar\
  \ med en webbserver, vanligtvis f\xF6r att h\xE4mta eller skicka data."
title: "Skicka en http-f\xF6rfr\xE5gan"
weight: 44
---

## Vad & Varför?
Att skicka en HTTP-begäran innebär att din applikation kommunicerar med en webbserver, vanligtvis för att hämta eller skicka data. Programmerare gör det för att interagera med webbtjänster, hantera API:er och så klart, för att internet skulle vara ganska värdelöst utan den här funktionaliteten.

## How to:
C++20 introducerade `<chrono>` och `<format>` biblioteken som ger en modern touch till hantering av tid och strängformatering.

```C++
#include <iostream>
#include <string>
#include <curl/curl.h> // Du måste ha libcurl installerat

static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp) {
    ((std::string*)userp)->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL *curl;
    CURLcode res;
    std::string readBuffer;

    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://httpbin.org/get");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        
        res = curl_easy_perform(curl);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        
        std::cout << readBuffer << std::endl;
        curl_easy_cleanup(curl);
    }
    curl_global_cleanup();
    return 0;
}
```

Exempel output:
```
{
  "args": {}, 
  "headers": {
    "Accept": "*/*", 
    "Host": "httpbin.org", 
    ...
  }, 
  "url": "https://httpbin.org/get"
}
```

## Deep Dive
Innan `libcurl`, alternativ som `WinInet` och `WinHttp` på Windows eller `CFNetwork` på MacOS var vanliga. Men `libcurl` ger en plattformsoberoende, omfattande lösning som fungerar överallt.

Fram till C++11 var det ingen standardiserad HTTP-klient i C++. `Boost.Asio` (nu en del av standardbiblioteket som `<asio>`) tillät nätverkskommunikation på lägre nivå men inte specialbyggd för HTTP.

`libcurl` är enkel att använda för alla HTTP-handlingar med stöd för FTP, SMTP och mer, medan modern C++ erbjuder bra verktyg för att hantera stringar och andra data som rör HTTP-kommunikation.

## See Also
- cURL officiell webbplats: https://curl.se/
- C++ Networking TS (Nätverkstekniska standarden): https://en.cppreference.com/w/cpp/experimental/net
- Modern C++ för nätverksprogrammering (bok): http://shop.oreilly.com/product/0636920047395.do
