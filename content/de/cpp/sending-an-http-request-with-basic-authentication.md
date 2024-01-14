---
title:                "C++: Versenden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Versenden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Das Versenden von HTTP-Anfragen mit grundlegender Authentifizierung ist ein wichtiger Aspekt der Webentwicklung. Durch die Nutzung der grundlegenden Authentifizierung können Entwickler sicherstellen, dass nur autorisierte Benutzer auf ihre Anwendung zugreifen können. In diesem Blogbeitrag werden wir uns genauer ansehen, wie Sie solche Anfragen in C++ programmieren können.

## Wie geht das

Um eine HTTP-Anfrage mit grundlegender Authentifizierung in C++ zu senden, müssen wir zunächst unsere Headers festlegen und eine Verbindung zu der gewünschten URL herstellen. Anschließend können wir die grundlegende Authentifizierung mit den Anmeldedaten des autorisierten Benutzers hinzufügen. Hier ist ein Beispielcode, der diesen Vorgang zeigt:

```C++
#include <iostream>
#include <curl/curl.h>

int main(void) {
  // Verbindung zu gewünschter URL herstellen
  CURL *curl;
  CURLcode res;
  curl = curl_easy_init();
  curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/secure-page.html");

  // Header für grundlegende Authentifizierung festlegen
  curl_slist *headers = NULL;
  headers = curl_slist_append(headers, "Content-Type: application/json");
  headers = curl_slist_append(headers, "Authorization: Basic dXNlcm5hbWU6cGFzc3dvcmQ=");

  // Authentifizierung hinzufügen und Anfrage senden
  curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
  res = curl_easy_perform(curl);

  curl_easy_cleanup(curl);
  return 0;
}
```

In diesem Beispiel haben wir die URL "https://www.example.com/secure-page.html" als Beispiel genommen und die Anmeldedaten "username:password" entsprechend codiert und als Teil des Authorization-Headers angegeben. Beachten Sie, dass für die grundlegende Authentifizierung auch andere Codierungsmethoden verwendet werden können.

Die Ausgabe des oben genannten Codes wird die gewünschte URL-Antwort enthalten, wenn die Authentifizierung erfolgreich war.

## Tiefer Einblick

Es ist wichtig zu verstehen, dass grundlegende Authentifizierung im Vergleich zu anderen Formen der Authentifizierung wie zum Beispiel OAuth nicht sehr sicher ist, da die Anmeldedaten nicht verschlüsselt werden. Es ist daher ratsam, grundlegende Authentifizierung nur unter Verwendung von HTTPS zu verwenden.

Außerdem ist es hilfreich zu wissen, dass die grundlegende Authentifizierung auch für andere HTTP-Anforderungen wie PUT, DELETE, etc. verwendet werden kann, indem dieselben Schritte wie im Beispielcode befolgt werden.

## Siehe auch

- [C++ HTTP-Anfragen mit Curl](https://curl.haxx.se/libcurl/c/example.html)
- [Grundlegende Authentifizierung in HTTP](https://developer.mozilla.org/de/docs/Web/HTTP/Authentication#Basic_Authentication)