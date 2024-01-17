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

# Was & Warum?

Das Senden einer HTTP-Anfrage (Hypertext Transfer Protocol) ist ein wesentlicher Bestandteil der Webprogrammierung. Es ermöglicht es Programmen, Daten von entfernten Servern zu erhalten und zu senden. Dies kann Typen wie Text, Bilder oder JSON enthalten. Programmierer verwenden dieses Werkzeug, um mit APIs zu interagieren, Webseiten zu laden und vieles mehr.

# Wie?

Um eine HTTP-Anfrage in C zu senden, verwenden wir die Bibliothek <curl.h>. Wir schaffen eine Verbindung zum entfernten Server und geben eine Zeichenfolge mit der gewünschten Aktion ein. Hier ist ein Beispiel:

```C
#include <curl/curl.h>

int main(void) {
  CURL *curl;
  CURLcode res;
  char *url = "https://example.com";

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, url);
    res = curl_easy_perform(curl);
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

Dieses Beispiel erstellt eine Verbindung zu "https://example.com" und führt eine GET-Anfrage aus. Die Antwort des Servers wird entweder geschrieben oder der Fehler wird ausgegeben.

# Tiefer Einblick

Das erste Werkzeug, das für die Übermittlung von HTTP-Anfragen verwendet wurde, war das "Telnet"-Protokoll. Es wurde in den 1960er Jahren entwickelt und ermöglichte es Benutzern, Befehle direkt an entfernte Server zu senden. In den 1990er Jahren wurde das HTTP-Protokoll entwickelt, das speziell für die Übermittlung von Webdaten entwickelt wurde.

Es gibt mehrere Alternativen zu <curl.h>, wie z.B. die Verwendung von "Sockets" direkt in C oder die Verwendung verbreiteter Webframeworks wie "Java Spring".

Bei der Übermittlung von HTTP-Anfragen gibt es einige Details zu beachten, z.B. die Verwendung von Zertifikaten für sichere Verbindungen, die Verwendung von "Headers" für die Übermittlung von Informationen und die Unterscheidung zwischen GET- und POST-Anfragen.

# Siehe auch

- Offizielle Dokumentation von <curl.h>: https://curl.haxx.se/libcurl/
- HTTP-Telnet-Client: https://curl.haxx.se/docs/httpscripting.html