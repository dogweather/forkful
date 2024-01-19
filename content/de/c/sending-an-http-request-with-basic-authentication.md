---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Versenden einer HTTP-Anforderung mit Basisauthentifizierung ist der Prozess, ein `GET` oder `POST` Request an einen Server zu senden, der ein `username:password` Paar im `Authorization` Header erfordert. Dies wird oft benutzt, um sensible Daten aus einem sicheren Server zu lesen oder dorthin zu schreiben.

## Wie geht das:

Wir verwenden die Bibliothek `libcurl` in C für dieses Beispiel. 

```C 
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        struct curl_slist *headers = NULL;
        headers = curl_slist_append(headers, "Authorization: Basic bXl1c2VyOm15cGFzc3dvcmQ=");

        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
        curl_easy_setopt(curl, CURLOPT_URL, "http://meinserver.de/seite");

        res = curl_easy_perform(curl);

        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() fehlgeschlagen: %s\n",
                curl_easy_strerror(res));

        curl_easy_cleanup(curl);
        curl_slist_free_all(headers);
    }

    curl_global_cleanup();

    return 0;
}
```
Diese Code sendet eine GET-Anforderung an `http://meinserver.de/seite` mit der Basisauthentifizierung `myuser:mypass`.

## Tiefere Einblicke:

Historisch gesehen, ist die Basisauthentifizierung der ursprüngliche und einfachste Standard für die HTTP-Authentifizierung. Es gibt jedoch alternative Methoden wie Digest Access Authentication oder Bearer Tokens. 

Während die Basisauthentifizierung immer noch weit verbreitet ist, hat sie einige Nachteile. Beispielsweise werden die Anmeldeinformationen unverschlüsselt (obwohl basis64-kodiert) übertragen, was bei ungesicherten Verbindungen zu Sicherheitsproblemen führen kann. Deshalb wird dringend empfohlen, sie zusammen mit HTTPS zu verwenden.

In der `libcurl` Bibliothek könnten Sie anstelle der `curl_easy_setopt()` Funktion die `curl_easy_setopt()` mit dem CURLAUTH_DIGEST-Flag verwenden, um die Digest Access Authentication zu implementieren, oder das `CURLOPT_XOAUTH2_BEARER`-Flag setzen, um einen OAuth 2.0 Bearer Token für die Authentifizierung zu verwenden.

## Weiterführende Informationen:

- [libcurl Dokumentation](https://curl.se/libcurl/)
- [RFC 2617 - HTTP Authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)
- [OAuth 2.0 Bearer Token Usage](https://tools.ietf.org/html/rfc6750)