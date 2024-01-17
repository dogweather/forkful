---
title:                "Senden einer HTTP-Anfrage mit Basic-Authentifizierung"
html_title:           "C: Senden einer HTTP-Anfrage mit Basic-Authentifizierung"
simple_title:         "Senden einer HTTP-Anfrage mit Basic-Authentifizierung"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Senden einer HTTP-Anfrage mit grundlegender Authentifizierung bedeutet, dass ein Programmierer bei der Verbindung mit einem Webserver Benutzername und Passwort verwendet, um sich zu authentifizieren. Dies wird in der Regel verwendet, um speziell geschützte Ressourcen oder eine API aufzurufen. Programmierer verwenden dies, um sicherzustellen, dass nur berechtigte Benutzer auf bestimmte Informationen zugreifen können.

# Wie Geht Das?

Der folgende Code zeigt ein Beispiel, wie man eine HTTP-Anfrage mit grundlegender Authentifizierung in C senden kann:

```
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;
  
  curl = curl_easy_init();
  
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://www.example.com/api");
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
    curl_easy_setopt(curl, CURLOPT_USERNAME, "username");
    curl_easy_setopt(curl, CURLOPT_PASSWORD, "password");
    
    res = curl_easy_perform(curl);
    
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
              
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

Das obige Beispiel verwendet die cURL-Bibliothek, um eine HTTP-Anfrage mit grundlegender Authentifizierung zu senden. Die Funktion ```curl_easy_setopt()``` wird verwendet, um die entsprechenden Optionen zu setzen, einschließlich der URL, der Art der Authentifizierung und der Benutzerdaten. Dann wird die Funktion ```curl_easy_perform()``` verwendet, um die Anforderung tatsächlich auszuführen.

Das Ergebnis sollte der erhaltene HTTP-Statuscode und der entsprechende Inhalt sein.

# Tiefer Einblick

Die grundlegende Authentifizierung wurde eingeführt, um eine einfache Methode zur Authentifizierung von Anfragen zu bieten. Sie ist jedoch nicht sehr sicher, da Benutzername und Passwort in Klartext übertragen werden. Aus diesem Grund werden heute oft alternative Methoden wie OAuth oder Token-basierte Authentifizierung verwendet.

Die cURL-Bibliothek ist eine beliebte Wahl zur Durchführung von HTTP-Anfragen und bietet eine Vielzahl von Optionen für verschiedene Arten von Authentifizierung.

# Siehe Auch

- [cURL-Bibliothek](https://curl.haxx.se/libcurl/)
- [OAuth](https://oauth.net/)
- [Token-basierte Authentifizierung](https://stormpath.com/blog/token-based-authentication-for-single-page-applications)