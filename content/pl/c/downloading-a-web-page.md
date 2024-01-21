---
title:                "Pobieranie strony internetowej"
date:                  2024-01-20T17:43:40.236726-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pobieranie strony internetowej"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
("## Co i dlaczego?")
Pobieranie strony internetowej to proces ściągania jej danych na lokalny komputer. Programiści robią to, aby przetwarzać informacje, automatyzować zadania, czy testować aplikacje sieciowe.

## How to:
("## Jak to zrobić:")
Użyjemy biblioteki libcurl, która umożliwia łatwe pobieranie stron. Poniższy kod demonstruje, jak to zrobić:

```c
#include <stdio.h>
#include <curl/curl.h>

size_t write_callback(void *contents, size_t size, size_t nmemb, void *userp) {
    size_t real_size = size * nmemb;
    printf("%.*s", (int)real_size, (char *)contents);
    return real_size;
}

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();

    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
        res = curl_easy_perform(curl);

        if(res != CURLE_OK) fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

        curl_easy_cleanup(curl);
    }

    curl_global_cleanup();

    return 0;
}
```

Po kompilacji i uruchomieniu program wypisze na ekranie zawartość strony http://example.com.

## Deep Dive:
("## W głąb tematu:")
Libcurl jest biblioteką klienta URL działającą od 2001 roku. Jest przeznaczona do transferu plików przy użyciu różnych protokołów i jest często wybierana przez programistów ze względu na jej wszechstronność i wsparcie dla różnych języków programowania. Alternatywnie, istnieje wiele innych narzędzi jak wget lub biblioteki języka C do obsługi HTTP, ale libcurl jest jedną z najbardziej wszechstronnych. Pamiętaj: zarządzanie pamięcią i bezpieczne obsługiwanie błędów są kluczowe przy pobieraniu danych z sieci.

## See Also:
("## Zobacz również:")
- Dokumentacja libcurl: https://curl.se/libcurl/
- Tutorial libcurl dla C: https://curl.se/libcurl/c/libcurl-tutorial.html
- Wprowadzenie do protokołów sieciowych: https://beej.us/guide/bgnet/
- Porównanie narzędzi do transferu danych: https://www.slant.co/topics/2676/~best-command-line-http-clients