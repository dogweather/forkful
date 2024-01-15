---
title:                "Senden einer http-Anfrage mit grundlegender Authentifizierung"
html_title:           "C: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man sich beim Senden einer HTTP-Anfrage mit Basissauthentifizierung (Basic Authentication) beschäftigen könnte. Möglicherweise möchten Sie eine sichere Verbindung zu einem Server herstellen oder Zugriff auf bestimmte geschützte Ressourcen erhalten. In jedem Fall ist es wichtig zu wissen, wie man eine Anfrage mit Basissauthentifizierung richtig sendet, um Ihre Ziele zu erreichen.

## Wie funktioniert es?

Um eine HTTP-Anfrage mit Basissauthentifizierung zu senden, müssen Sie Ihrem Code einige zusätzliche Parameter hinzufügen. Hier ist eine Beispielfunktion in C, die Ihnen zeigt, wie Sie das tun können:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  // Initialisieren Sie den Curl-Handle
  curl = curl_easy_init();
  if(curl) {
    // Setzen Sie die zu verwendende URL
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/");

    // Fügen Sie den Benutzernamen und das Passwort hinzu
    curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

    // Führen Sie die HTTP-Anfrage aus und speichern Sie die Antwort in 'res'
    res = curl_easy_perform(curl);

    // Überprüfen Sie, ob Fehler aufgetreten sind
    if(res != CURLE_OK)
      fprintf(stderr, "curl Fehler: %s\n",
              curl_easy_strerror(res));

    // Bereinigen Sie den Curl-Handle
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

Dieses Beispiel verwendet die Curl-Bibliothek, um eine HTTP-Anfrage durchzuführen. Es verwendet auch die Funktion `curl_easy_setopt()` , um zusätzliche Parameter wie die URL und die Benutzerdaten anzugeben. Wenn alles gut geht, sollte die Antwort in der Variablen `res` gespeichert werden.

## Tiefergehende Einblicke

Beim Senden einer HTTP-Anfrage mit Basissauthentifizierung ist es wichtig zu beachten, dass der Benutzername und das Passwort im Klartext übertragen werden. Aus diesem Grund sollte diese Methode nur für sichere Verbindungen verwendet werden und das Passwort sollte regelmäßig geändert werden.

Es gibt auch verschiedene Arten von HTTP-Authentifizierung, wie Digest- und OAuth-Authentifizierung, die möglicherweise besser geeignet sind, je nachdem, was Sie erreichen möchten. Es lohnt sich, sich mit diesen Alternativen vertraut zu machen, um die beste Wahl für Ihr Szenario zu treffen.

Insgesamt ist das Senden einer HTTP-Anfrage mit Basissauthentifizierung eine nützliche Technik, um Zugriff auf geschützte Ressourcen zu erhalten. Mit den richtigen Werkzeugen und Kenntnissen können Sie sicher und effektiv mit Ihrer Ziel-URL kommunizieren.

## Siehe auch

- [Curl-Bibliothek](https://curl.se/libcurl/)
- [HTTP-Authentifizierungstypen](https://developer.mozilla.org/de/docs/Web/HTTP/Authentication)