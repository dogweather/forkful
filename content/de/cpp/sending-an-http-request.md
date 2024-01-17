---
title:                "Das Senden einer http-Anfrage"
html_title:           "C++: Das Senden einer http-Anfrage"
simple_title:         "Das Senden einer http-Anfrage"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Senden einer HTTP Anfrage ist der Prozess, bei dem ein Client mit einem Server kommuniziert und Daten austauscht. Programme führen dies aus, um zum Beispiel Webseiten oder API-Daten abzurufen.

## Wie geht's:
Zuerst müssen wir eine HTTP-Verbindung aufbauen, indem wir eine Verbindung zu einer URL herstellen. Dann können wir mit Hilfe von HTTP-Methoden wie GET, POST, PUT oder DELETE Daten senden. Schauen wir uns ein Beispiel an, wie man mit der libcurl Bibliothek eine HTTP Anfrage sendet:

```C++
#include <curl/curl.h>

// Funktion, um eine HTTP Anfrage zu senden
int sendHTTPRequest() {
  // Initialisiere Curl
  CURL *curl;
  curl = curl_easy_init();
  // Setze die URL
  curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/api/data");
  // Definiere die Methode (in diesem Fall POST)
  curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "POST");
  // Füge Daten hinzu
  curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "username=test&password=pass");
  // Führe die Anfrage aus
  CURLcode res = curl_easy_perform(curl);
  // Überprüfe auf Fehler
  if(res != CURLE_OK) {
    // Gib Fehlermeldung aus
    fprintf(stderr, "curl_easy_perform() failed: %s\n",
            curl_easy_strerror(res));
  }
  // Gib den Speicher wieder frei
  curl_easy_cleanup(curl);
  return 0;
}

// Funktionsaufruf
int main() {
  sendHTTPRequest();
  return 0;
}
```

Die Ausgabe des Codes wird je nach Server und Daten, die gesendet werden, variieren. Eine erfolgreiche Anfrage gibt normalerweise den Statuscode "200 OK" und ggf. die angeforderten Daten zurück.

## Tieferes Eintauchen:
Das Konzept des Sendens von HTTP Anfragen wurde ursprünglich in den frühen 90er Jahren entwickelt und ist ein wichtiger Teil des World Wide Web. Es gibt verschiedene Alternativen zu libcurl wie z.B. die Boost Asio Bibliothek oder die Standardbibliothek in C++, die auch zur Kommunikation mit Servern verwendet werden können. Bei der Implementierung einer HTTP Anfrage müssen auch die Sicherheitsaspekte berücksichtigt werden, da unverschlüsselte Anfragen sensible Daten öffentlich preisgeben könnten.

## Siehe auch:
- [Offizielle libcurl Dokumentation](https://curl.haxx.se/libcurl/)
- [Boost Asio Bibliothek](https://www.boost.org/doc/libs/1_68_0/doc/html/boost_asio.html)
- [Standardbibliothek in C++](http://en.cppreference.com/w/cpp/net/http)