---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ladda ner en webbsida betyder att kopiera den för offline-användning. Programmerare gör detta för att analysera data, testa funktionalitet eller hitta buggar.

## Hur man gör:

För att ladda ner en sida i C++, kan vi använda libcurl bibliotek. Här är ett exempel:

```C++
#include <curl/curl.h>
#include <iostream>
#include <string>

size_t WriteCallback(void* contents, size_t size, size_t nmemb, std::string* userp) {
    userp->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL* curl;
    CURLcode res;
    std::string readBuffer;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);
        std::cout << readBuffer << std::endl;
    }
    return 0;
}
```

Om du kör programmet bör du se innehållet på http://example.com skrivet till konsolen.

## Djupdykning

Först, programmets historiska sammanhang: libcurl, släpptes 1997, har blivit en standard för att hantera URL-baserade operationer i C++.

Andra metoder: En annan populär metod för att ladda ner webbsidor i C++ innebär användning av Boost.Asio med HTTP-klientexempel.

Vad gäller implementeringsdetaljerna, tar libcurl hand om mycket bakom kulisserna. Det stöder massor av protokoll (HTTP, HTTPS, FTP, SFTP, etc), och hanterar fel, omdirigering, och mycket mer.

## Se också

- *[libcurl Documentation](https://curl.haxx.se/libcurl/)*: För mer information om att använda libcurl.
- *[Boost.Asio HTTP Client Example](https://www.boost.org/doc/libs/1_66_0/doc/html/boost_asio/example/cpp11/http/client/async_client.cpp)*: En mer avancerad metod för att ladda ner webbsidor i C++. 
- *[Stackoverflow](https://stackoverflow.com/questions/1011339/how-do-you-make-a-http-request-with-c)*: Diskussioner om olika sätt att utföra HTTP-anrop i C++.