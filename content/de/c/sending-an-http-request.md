---
title:                "Eine http-Anfrage senden"
html_title:           "C: Eine http-Anfrage senden"
simple_title:         "Eine http-Anfrage senden"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich die Mühe machen, ein HTTP Request in C zu senden? Nun, wenn du jemals eine Webanwendung oder API programmieren musst, wirst du fast immer HTTP Requests benötigen. Diese ermöglichen es dir, Daten von einem Server zu erhalten oder an einen Server zu senden, was für die meisten Websites und Anwendungen unerlässlich ist. Also lass uns herausfinden, wie man das in C macht!

## How To

Der erste Schritt, um einen HTTP Request in C zu senden, ist das Einbinden der `curl` Bibliothek. Diese Bibliothek ermöglicht es uns, HTTP-ähnliche Requests auf einer Ebene über der Sockets API durchzuführen. Hier ist ein Beispiel, wie man eine einfache GET Request an eine URL sendet:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;
  char *url = "https://example.com/api/data";

  curl = curl_easy_init();    //initialisiere curl
  if(curl) {
    //setze die URL
    curl_easy_setopt(curl, CURLOPT_URL, url);
    //führe die GET Request aus
    res = curl_easy_perform(curl);
    //überprüfe auf Fehler
    if(res != CURLE_OK)
      fprintf(stderr, "curl Fehler: %s\n",
              curl_easy_strerror(res));
    //schließe curl
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

Die Ausgabe wird je nach URL variieren, aber du solltest ein HTML-Dokument sehen, das die Daten des Servers enthält.

## Deep Dive

Nun, das war eine sehr einfache GET Request. Aber was ist, wenn du Daten an den Server senden oder spezifischere Informationen in deinem Request haben möchtest? Dafür gibt es verschiedene Optionen, die alle mit der `curl_easy_setopt` Funktion gesetzt werden können.

Du kannst z.B. einen POST Request senden, indem du die Einstellung `CURLOPT_POSTFIELDS` auf die Daten setzt, die du senden möchtest. Du kannst auch die Header deines Requests mit `CURLOPT_HTTPHEADER` anpassen, um spezifische Anforderungen zu erfüllen.

Eine vollständige Liste aller Optionen findest du in der `curl` Dokumentation.

## Siehe auch

- Offizielle `curl` Dokumentation: https://curl.haxx.se/libcurl/c/
- Übersicht über HTTP Requests in C: https://www.hackerearth.com/practice/notes/http-request-in-c/
- Beispielcode für verschiedene Arten von HTTP Requests in C: https://curl.haxx.se/libcurl/c/example.html