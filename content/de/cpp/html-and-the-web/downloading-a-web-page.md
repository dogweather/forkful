---
aliases:
- /de/cpp/downloading-a-web-page/
date: 2024-01-20 17:43:32.434171-07:00
description: "Das Herunterladen einer Webseite bedeutet, ihren Inhalt \xFCber das\
  \ Internet abzurufen. Programmierer tun das, um Daten zu sammeln, Webservices zu\
  \ nutzen\u2026"
lastmod: 2024-02-18 23:09:05.187771
model: gpt-4-1106-preview
summary: "Das Herunterladen einer Webseite bedeutet, ihren Inhalt \xFCber das Internet\
  \ abzurufen. Programmierer tun das, um Daten zu sammeln, Webservices zu nutzen\u2026"
title: Webseite herunterladen
---

{{< edit_this_page >}}

## Was & Warum?
Das Herunterladen einer Webseite bedeutet, ihren Inhalt über das Internet abzurufen. Programmierer tun das, um Daten zu sammeln, Webservices zu nutzen oder Inhalte zu analysieren.

## How to:
Hier ist ein einfaches Beispiel mit `C++` und `libcurl`, einer leistungsfähigen Bibliothek für Client-seitige URL-Transfers.

```cpp
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
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        }
        curl_easy_cleanup(curl);

        std::cout << readBuffer << std::endl;
    }
    
    return 0;
}
```

Wenn alles klappt, siehst du den HTML-Inhalt von `http://example.com`.

## Deep Dive:
Anfangs waren die Werkzeuge zum Herunterladen von Webseiten limitiert. `libcurl` wurde 1997 veröffentlicht und hat sich seitdem als robuste und vielseitige Bibliothek etabliert. Alternativ könnten Programmierer auch Sprachen wie Python verwenden oder Tools wie `wget` oder `curl` in einem Terminal. Die Herausforderung bei C++ ist die Fehlerbehandlung und die Notwendigkeit für eine externe Bibliothek, da Standard-C++ keine eingebauten HTTP-Handling-Funktionen hat.

## See Also:
- [libcurl documentation](https://curl.se/libcurl/c/)
- [HTTP Protokoll](https://tools.ietf.org/html/rfc2616)
