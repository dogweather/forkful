---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# HTTP-Anfrage mit Basic-Authentifizierung in C++

## Was & Warum?

Eine HTTP-Anfrage mit Basic-Authentifizierung erlaubt es einem Client, sich über ein Benutzername-Passwort-Paar an einem Server zu authentifizieren. Programmierer tun dies, um sicherzustellen, dass nur autorisierte Benutzer Zugriff auf bestimmte Ressourcen haben.

## Anleitung:

In C++ könnten wir die Bibliothek libcurl verwenden, um eine HTTP-Anfrage mit Basic-Authentifizierung zu senden. Erstellen wir einen einfachen Code:

```C++
#include <curl/curl.h>

int main() {

    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

        curl_easy_setopt(curl, CURLOPT_USERNAME, "username");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");

        res = curl_easy_perform(curl);

        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();
    
    return 0;
}
```

Dieser Code würde eine GET-Anfrage an http://example.com mit dem Benutzernamen "username" und dem Passwort "password" senden.

## Vertiefung

Basic-Authentifizierung ist ein alter Mechanismus, der 1995 mit der Veröffentlichung von HTTP/1.0 eingeführt wurde. Es gibt viele Alternativen dazu, wie OAuth und JWT, die sicherer und flexibler sind, aber Basic-Authentifizierung ist immer noch in Gebrauch, meist wegen seiner Einfachheit.

In Bezug auf die Implementierung wird das Benutzername-Passwort-Paar zu einem String "username:password" konvertiert, dann wird dieser String Base64-kodiert und in den Authorization-Header der HTTP-Anfrage eingefügt, sodass es so aussieht: "Authorization: Basic base64(username:password)".

## Weiterführende Links

1. libcurl Dokumentation: https://curl.haxx.se/libcurl/c/
2. HTTP-Basic-Authentifizierung: https://developer.mozilla.org/de/docs/Web/HTTP/Authentication
3. Verwendung von libcurl mit SSH-Support in C++: https://curl.haxx.se/libcurl/c/Using-libcurl-with-SSH-support-in-Cplusplus.html