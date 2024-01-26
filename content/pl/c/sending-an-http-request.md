---
title:                "Wysyłanie żądania HTTP"
date:                  2024-01-20T17:59:04.223672-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Wysyłanie żądania HTTP pozwala naszemu programowi komunikować się z serwerem webowym - wymieniać dane, pobierać strony internetowe czy korzystać z API. Programiści robią to, aby integrować aplikacje C z internetem, zbierać informacje lub interagować z usługami webowymi.

## How to: (Jak to zrobić?)
Użycie biblioteki cURL w C do wysłania żądania HTTP jest proste. Oto przykład:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_ALL);

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        // Pełni rolę użytkownika przeglądarki
        curl_easy_setopt(curl, CURLOPT_USERAGENT, "libcurl-agent/1.0");

        // Wykonanie żądania, res przechwytuje wynik
        res = curl_easy_perform(curl);

        // Sprawdzenie błędów
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));

        // Zawsze czyść po sobie
        curl_easy_cleanup(curl);
    }

    curl_global_cleanup();

    return 0;
}
```

Po uruchomieniu, program wysyła żądanie GET do `http://example.com` i wypisuje odpowiedź.

## Deep Dive (Dogłębna analiza)
Wysyłanie żądań HTTP z C nie zawsze było tak łatwe. W przeszłości trzeba było ręcznie tworzyć gniazda i obsługiwać protokół HTTP. Biblioteka cURL, wydana w 1997 roku, uprościła proces poprzez dostarczenie prostego API.

Alternatywami dla cURL są libwww, Qt network module, czy Boost.Asio dla C++. Współczesne implementacje opierają się na wygodzie użytkowania i bezpieczeństwie; wiele z nich dba o zarządzanie pamięcią i automatyczne obsługiwanie certyfikatów SSL/TLS.

## See Also (Zobacz również)
- Dokumentacja cURL: https://curl.haxx.se/libcurl/c/
- Wstęp do gniazd w C: https://beej.us/guide/bgnet/
- Informacje o protokole HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP
