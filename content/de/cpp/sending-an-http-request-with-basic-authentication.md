---
title:                "HTTP-Anfragen mit Basisauthentifizierung senden"
aliases:
- de/cpp/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:01.526459-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-Anfragen mit Basisauthentifizierung senden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Senden einer HTTP-Anfrage mit Basis-Authentifizierung bedeutet, dass der Client seine Anmeldeinformationen (Benutzername und Passwort) kodiert im HTTP-Header mitschickt. Programmierer nutzen dies oft, um auf geschützte Ressourcen zuzugreifen, die eine Authentifizierung erfordern.

## Vorgehensweise:
```C++
#include <iostream>
#include <string>
#include <curl/curl.h>

size_t callback(char* buf, size_t size, size_t nmemb, void* up) {
    for (int c = 0; c < size*nmemb; c++) {
        std::cout << buf[c];
    }
    return size * nmemb;
}

int main() {
    CURL* curl = curl_easy_init();
    if(curl) {
        std::string userAndPassword = "user:password"; // Ersetzen Sie dies durch Ihre echten Anmeldeinformationen.
        std::string authHeader = "Authorization: Basic " + userAndPassword;

        struct curl_slist *headers = nullptr;
        headers = curl_slist_append(headers, authHeader.c_str());

        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/protected"); // URL der geschützten Ressource
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, callback);
        
        CURLcode res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            std::cerr << "CURL error: " << curl_easy_strerror(res) << std::endl;
        }
        curl_slist_free_all(headers);
        curl_easy_cleanup(curl);
    }
    return 0;
}
```
**Achtung**: `user:password` muss in Base64 kodiert werden. Verwenden Sie den Header, wie er ist, nur mit Ihren echten Basis-Authentifizierungsinformationen.

Output (hängt von der Ressource ab):

```
HTTP/1.1 200 OK
...
Hello, secured world!
```

## Deep Dive
Die Basis-Authentifizierung ist ein seit langem bestehender Mechanismus und wird wegen ihrer Einfachheit oft verwendet. Die Anmeldeinformationen sind lediglich Base64-kodiert, jedoch nicht verschlüsselt, was sie anfällig für Man-in-the-Middle-Angriffe macht. Daher sollte sie zusammen mit HTTPS eingesetzt werden.

Ein alternatives Authentifizierungsschema könnte OAuth sein, das sicherer ist und mehr Flexibilität bei der Zugriffskontrolle bietet.

Die verwendete LIBCURL-Bibliothek in diesem Beispiel ist eine robuste, portierbare Bibliothek, die den Umgang mit HTTP-Anfragen einfach gestaltet. Sie kümmert sich um Networking-Aufgaben, sodass der Entwickler sich auf das Wesentliche konzentrieren kann.

## Siehe auch
- [cURL libcurl - API Dokumentation](https://curl.se/libcurl/c/)
- [HTTP-Authentifizierung auf MDN](https://developer.mozilla.org/de/docs/Web/HTTP/Authentication)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://datatracker.ietf.org/doc/html/rfc7617)
