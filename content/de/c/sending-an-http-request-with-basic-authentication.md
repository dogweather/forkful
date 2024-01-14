---
title:                "C: Versenden einer HTTP-Anfrage mit Grundauthentifizierung"
simple_title:         "Versenden einer HTTP-Anfrage mit Grundauthentifizierung"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Warum
HTTP-Anfragen mit Grundauthentifizierung werden häufig verwendet, um vertrauliche Informationen sicher zu übertragen und den Zugriff auf geschützte Ressourcen zu kontrollieren. Dieses Blog-Post erklärt, wie man in C eine HTTP-Anfrage mit Grundauthentifizierung senden kann.

# Wie geht das?
Um eine HTTP-Anfrage mit Grundauthentifizierung in C zu senden, müssen zuerst die erforderlichen Header-Informationen erstellt werden. Dazu gehört ein Header für die Autorisierung, der Benutzername und Passwort in Base64-kodierter Form enthält. Hier ist ein Beispielcode, der eine HTTP-Anfrage an einen Server mit Grundauthentifizierung sendet:

```C
#include<stdio.h>
#include<curl/curl.h>

int main(void)
{
    CURL *curl;
    CURLcode res;

    // Initialisieren des CURL-Objekts
    curl = curl_easy_init();
    if(curl) {
        // Setzen der URL für die Anfrage
        curl_easy_setopt(curl, CURLOPT_URL, "http://www.example.com/resource");
        
        // Setzen der Option für Grundauthentifizierung
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        
        // Setzen des Benutzernamens
        curl_easy_setopt(curl, CURLOPT_USERNAME, "benutzername");
        
        // Setzen des Passworts
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "passwort");

        // Ausführen der Anfrage
        res = curl_easy_perform(curl);
        
        if(res != CURLE_OK)
            printf("Fehler bei der Anfrage: %s\n", curl_easy_strerror(res));
        
        // Aufräumen nach der Anfrage
        curl_easy_cleanup(curl);
    }

    return 0;
}
```

Die Ausgabe dieses Codes sollte eine Antwort von dem Server mit Statuscode 200 "OK" in der Konsole ausgeben. Dies bedeutet, dass die Anfrage erfolgreich gesendet wurde.

# Tiefere Einblicke
Um eine HTTP-Anfrage mit Grundauthentifizierung zu senden, muss man verstehen, wie der Autorisierungsheader aufgebaut ist. Dieser besteht aus dem Wort "Basic" gefolgt von einem Leerzeichen und dann dem Base64-kodierten Benutzernamen und Passwort, getrennt durch einen Doppelpunkt. Zum Beispiel, wenn der Benutzername "benutzername" und das Passwort "passwort" ist, sieht der Autorisierungsheader wie folgt aus:

```
Authorization: Basic YmVudXRlc3Ryb246cGFzc3dvcnQ=
```

Base64 ist eine Kodierungsmethode, bei der Daten als ASCII-Zeichen dargestellt werden. Dies bedeutet, dass sie nicht als Klartext sichtbar sind und somit etwas sicherer sind.

Ein weiteres wichtiges Konzept zu beachten ist die Verwendung von HTTP-Header-Felder. Diese bestimmen, welche Art von Daten in einer HTTP-Anfrage oder -Antwort gesendet werden. In diesem Fall ist der Autorisierungsheader ein "Request Header"-Feld, da er angegeben wird, um die Anfrage zu authentifizieren.

# Siehe auch
- [Curl-Bibliothek](https://curl.se/libcurl/c)
- [Base64-Kodierung](https://de.wikipedia.org/wiki/Base64)
- [HTTP-Header-Felder](https://developer.mozilla.org/de/docs/Web/HTTP/Headers)