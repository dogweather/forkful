---
title:                "Hämta en webbsida"
aliases: - /sv/cpp/downloading-a-web-page.md
date:                  2024-01-20T17:43:38.444844-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hämta en webbsida"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida innebär att hämta sidans innehåll över nätet. Programmerare gör detta för att analysera innehållet, granska data eller integrera funktionalitet från externa webbplatser i sina applikationer.

## Så här gör du:
För att ladda ner en webbsida i C++ kan vi använda biblioteket cURL. Först måste du se till att cURL är installerat och länkat till ditt projekt.

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
        curl_easy_cleanup(curl);

        if(res == CURLE_OK) {
            std::cout << readBuffer << std::endl;
        }
    }
    return 0;
}
```
Kör programmet. Om allt är som det ska bör du se webbsidans HTML i konsolen.

## Fördjupning:
Tidigare använde många programmerare sockets och lågnivå-nätverksbibliotek för att ladda ner webbdata. cURL biblioteket är nu standard; det hanterar URL-syntax, protokoll och till och med SSL-certifikat transparent.

Alternativ till cURL inkluderar andra bibliotek som libwww eller högnivå C++ bibliotek som Boost.Beast, vilket också kan använda moderna C++ funktioner och asynkrona begärningar.

När du implementerar en nedladdning är det viktigt att hantera fel korrekt och respektera webbserverns ev. begränsningar, såsom att respektera `robots.txt`.

## Se även:
- cURL: https://curl.se/
- libcurl C++ wrapper-dokumentation: https://curl.se/libcurl/cplusplus/
- Boost.Beast GitHub-sida: https://github.com/boostorg/beast
- HTTP-protokollets specifikationer: https://www.ietf.org/rfc/rfc2616.txt
