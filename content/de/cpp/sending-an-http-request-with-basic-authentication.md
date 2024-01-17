---
title:                "Senden einer http-Anfrage mit grundlegender Authentifizierung"
html_title:           "C++: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP Anfragen mit grundlegender Authentifizierung zu senden bedeutet, dass man eine spezielle Anfrage an einen Webserver schickt, inklusive Nutzername und Passwort für den Server. Programmierer tun dies, um sich bei bestimmten Webdiensten oder APIs anzumelden und auf geschützte Ressourcen zuzugreifen.

## Wie geht's?
```C++
#include <iostream>
#include <curl/curl.h>

int main() {
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
    curl_easy_setopt(curl, CURLOPT_USERNAME, "username");
    curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");

    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }
  return 0;
}
```
**Output:**
`<!doctype html>
<html>
<head>
  <title>Example Domain</title>

  <meta charset="utf-8" />
  <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
</head>

<body>
<div>
  <h1>Example Domain</h1>
  <p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p>
  <p><a href="https://www.iana.org/domains/example">More information...</a></p>
</div>
</body>
</html>`

## Tief tauchen
Eine grundlegende Authentifizierung wurde bereits 1999 im RFC 2617 definiert und verwendet Base64- kodierten Nutzernamen und Passwörter. Eine alternative zur grundlegenden Authentifizierung ist die Digest Authentifizierung, welche stärkere Sicherheitsmaßnahmen bietet. Die Implementierung einer grundlegenden Authentifizierung erfordert die Nutzung von HTTP-Headern und kann in verschiedenen Programmiersprachen durchgeführt werden, nicht nur in C++.

## Siehe auch
[Offizielle cURL Dokumentation](https://curl.haxx.se/libcurl/c/curl_easy_setopt.html)

[HTTP Authentifizierung und Curl](https://curl.haxx.se/docs/httpauth.html)

[HTTP Grundlegendes Authentifizierungs RFC](https://tools.ietf.org/html/rfc2617)