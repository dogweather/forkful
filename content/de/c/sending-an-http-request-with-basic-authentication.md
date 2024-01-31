---
title:                "HTTP-Anfragen mit Basisauthentifizierung senden"
date:                  2024-01-20T18:01:04.163020-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-Anfragen mit Basisauthentifizierung senden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen mit Basisauthentifizierung senden heißt, einen sicheren Weg zu nutzen, um Benutzername und Passwort an einem Server zu übermitteln. Programmierer nutzen dies, um Zugang zu Webressourcen zu gewährleisten, die eine Authentifizierung erfordern.

## How to (Anleitung):
Die Bibliothek `libcurl` ist ein guter Startpunkt in C. Hier ein einfaches Beispiel:

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();
    
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/data");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "user");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "pass");
        
        res = curl_easy_perform(curl);
        
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();
    return 0;
}
```

Erwartete Ausgabe: Die auf der Konsole dargestellten Daten von `http://example.com/data`, sofern Benutzername und Passwort korrekt sind.

## Deep Dive (Tiefergehende Informationen):
Basisauthentifizierung ist Teil von HTTP, schon lange vorhanden, und wird über den `Authorization`-Header realisiert. Alternativen wie OAuth bieten stärkere Sicherheit. Trotzdem ist Basisauthentifizierung wegen ihrer Einfachheit beliebt. Beachte: Die credibilities sollten über HTTPS gesendet werden, um die Sicherheit zu gewährleisten. Die `libcurl`-Bibliothek kümmert sich fast um alles, was du brauchst, aber der Netzwerk-Übertragungsteil (z.B. SSL/TLS) hängt vom Setup deiner `libcurl`-Installation ab.

## See Also (Siehe auch):
- cURL Dokumentation: https://curl.haxx.se/libcurl/c/
- RFC 7617, 'The 'Basic' HTTP Authentication Scheme': https://tools.ietf.org/html/rfc7617
- HTTPS und Sicherheitsempfehlungen: https://www.owasp.org/index.php/Transport_Layer_Protection_Cheat_Sheet
