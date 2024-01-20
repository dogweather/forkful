---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

# HTTP-Anfrage in C: Was, Warum und wie man sie sendet

## Was & Warum?
Das Senden einer HTTP-Anfrage ist der Prozess der Kommunikation mit Webservern, um Informationen zu empfangen oder zu senden. Programmierer tun dies, um Aufgaben wie APIs abrufen, Daten von Websites extrahieren oder sogar Web-Scraping durchzuführen.

## Wie geht das?
Wir werden die Curl-Bibliothek verwenden, um HTTP-Anfragen in C zu senden. Hier ist ein einfaches Beispiel:

```C
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

    /* for secure connections */
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);

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
Dieses Programm ruft einfach eine Webseite ab und druckt ihren Inhalt in der Konsole aus.

## Deep Dive
HTTP-Anfragen haben eine lange Geschichte, die auf die Anfänge des Webs zurückgeht. Ursprünglich eine einfache Anforderungs-Antwort-Struktur, hat sich die Technologie entwickelt, um komplexere Daten und Formate zu verarbeiten. Alternativen zum Senden von HTTP-Anfragen in C umfassen die Verwendung anderer Bibliotheken wie WinINet oder die WebSocket-API für Echtzeitanwendungen. Die Implementierung hängt von der spezifischen Bibliothek ab, die Sie verwenden - Curl zum Beispiel bietet sowohl blockierende als auch nicht-blockierende Versionen seiner API an.

## Weitere Infos
Für weitere Informationen über das Senden von HTTP-Anfragen in C, schauen Sie bitte auf die folgenden Links:
- [Curl Official Documentation](https://curl.se/libcurl/c/)
- [HTTP Made Really Easy](http://www.jmarshall.com/easy/http/)
- [RFC 2616 - Hypertext Transfer Protocol -- HTTP/1.1](https://tools.ietf.org/html/rfc2616)