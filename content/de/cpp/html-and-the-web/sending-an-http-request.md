---
title:                "Einen HTTP-Request senden"
aliases:
- /de/cpp/sending-an-http-request/
date:                  2024-01-20T17:59:18.678074-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen HTTP-Request senden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen ermöglichen die Kommunikation mit Web-Diensten. Programmierer senden solche Anfragen, um Daten zu erhalten, zu senden oder Web-Operationen auszuführen.

## Vorgehensweise:
C++ hat keine eingebaute Unterstützung für HTTP. Bibliotheken wie `libcurl` oder `cpprestsdk` sind beliebt. Hier ein Beispiel mit `libcurl`.

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

        if(res == CURLE_OK)
            std::cout << readBuffer << std::endl;
        else
            std::cerr << "Fehler: " << curl_easy_strerror(res) << std::endl;
    }

    return 0;
}
```

Sample Output:
```
<!doctype html>
<html>
...
</html>
```

## Vertiefung:
HTTP-Anfragen sind seit dem frühen WWW in den 90er-Jahren Teil des Internets. Alternative Technologien wie gRPC übertragen Daten insbesondere in Microservices-Architekturen effizienter. Für C++-HTTP-Anfragen müssen Sie eine Drittbibliothek verwenden, wobei `libcurl` die direkteste ist und `cpprestsdk` objektorientierte und asynchrone Features bietet.

## Siehe auch:
- cURL Bibliothek: https://curl.haxx.se/libcurl/
- cpprestsdk (C++ REST SDK): https://github.com/Microsoft/cpprestsdk
- RFC 7230, Hypertext Transfer Protocol (HTTP/1.1): Message Syntax and Routing: https://tools.ietf.org/html/rfc7230
