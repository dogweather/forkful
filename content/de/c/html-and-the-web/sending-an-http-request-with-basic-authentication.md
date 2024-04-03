---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:00.064764-07:00
description: "Das Senden einer HTTP-Anfrage mit Basisauthentifizierung in C erfordert\
  \ das Erstellen einer HTTP-Anfrage, die einen Autorisierungsheader mit in Base64\u2026"
lastmod: '2024-03-13T22:44:54.352080-06:00'
model: gpt-4-0125-preview
summary: "Das Senden einer HTTP-Anfrage mit Basisauthentifizierung in C erfordert\
  \ das Erstellen einer HTTP-Anfrage, die einen Autorisierungsheader mit in Base64\
  \ kodierten Benutzerdaten enth\xE4lt."
title: Eine HTTP-Anfrage mit Basisauthentifizierung senden
weight: 45
---

## Was & Warum?
Das Senden einer HTTP-Anfrage mit Basisauthentifizierung in C erfordert das Erstellen einer HTTP-Anfrage, die einen Autorisierungsheader mit in Base64 kodierten Benutzerdaten enthält. Dies ist eine gängige Methode, um HTTP-Anfragen eine einfache Authentifizierungsschicht hinzuzufügen, die es ermöglicht, programmatisch auf beschränkte Ressourcen zuzugreifen.

## Wie man es macht:
Um eine HTTP-Anfrage mit Basisauthentifizierung in C zu senden, müssen wir die libcurl-Bibliothek verwenden, eine beliebte, vielseitige und einfach zu bedienende Client-seitige URL-Übertragungsbibliothek. Sie unterstützt verschiedene Protokolle, einschließlich HTTP und HTTPS, was unsere Aufgabe vereinfacht. Stellen Sie sicher, dass libcurl auf Ihrem System installiert ist, bevor Sie fortfahren. Hier ist ein einfaches Beispiel, das zeigt, wie man eine GET-Anfrage mit Basisauthentifizierung sendet:

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        // Die URL, an die die Anfrage gesendet wird
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        // Aktivierung der Verwendung der Basisauthentifizierung
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        // Bereitstellung des Benutzernamens und des Passworts für die Basisauthentifizierung
        curl_easy_setopt(curl, CURLOPT_USERPWD, "benutzername:passwort");

        // Durchführen der GET-Anfrage
        res = curl_easy_perform(curl);

        // Überprüfung auf Fehler
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() ist fehlgeschlagen: %s\n",
                    curl_easy_strerror(res));

        // Immer aufräumen
        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();

    return 0;
}
```
Im obigen Beispiel ersetzen Sie `"http://example.com/resource"`, `"benutzername"` und `"passwort"` durch Ihre tatsächliche URL, Benutzernamen und Passwort.

Dieser Code initialisiert ein `CURL`-Objekt, setzt die URL, aktiviert die HTTP-Basisauthentifizierung und gibt die Anmeldeinformationen an. Anschließend sendet er die Anfrage und räumt nach sich auf. Wenn erfolgreich, wird die angeforderte Ressource abgerufen; gibt es einen Fehler, wird dieser auf stderr ausgegeben.

Beispieloutput (unter der Annahme einer erfolgreichen Authentifizierung und Ressourcenzugriff) wird möglicherweise nicht direkt vom Programm angezeigt, da das Beispiel primär das Senden der Anfrage demonstriert. Um die Antwort auszugeben, würden Sie das Programm erweitern, um die HTTP-Antwortdaten zu verarbeiten.

## Tiefergehend:
Das Senden von HTTP-Anfragen mit Basisauthentifizierung in C, wie gezeigt, nutzt die libcurl-Bibliothek wegen ihrer Robustheit und Einfachheit. Historisch gesehen war das Erstellen von HTTP-Anfragen rein in C ohne derartige Bibliotheken mühsam und fehleranfällig, da dies eine Programmierung auf niedriger Ebene und manuelles Erstellen von HTTP-Headern erforderte.

Die Basisauthentifizierung selbst ist eine Methode aus den Anfangstagen des Webs. Sie sendet Anmeldeinformationen in einem leicht dekodierbaren Format (Base64), was über unverschlüsselte Kanäle inhärent unsicher ist. Moderne Anwendungen bevorzugen oft sicherere Authentifizierungsmethoden wie OAuth 2.0 oder JWT (JSON Web Tokens), besonders für sensible Daten.

Jedoch bleibt für interne, weniger kritische Systeme oder schnelle und schmutzige Skripte, wo Bequemlichkeit Sicherheitsbedenken überwiegt, die Basisauthentifizierung in Gebrauch. Darüber hinaus wird ihre Einfachheit zu einem Vorteil für schnelle Entwicklung, Tests oder Automatisierungsarbeiten, wenn höhere Sicherheitsmechanismen nicht so notwendig sind, wenn sie mit verschlüsselten Verbindungen (HTTPS) kombiniert wird.

In Kontexten, in denen hochmoderne Sicherheit unabdingbar ist, sollten Alternativen wie tokenbasierte Authentifizierung priorisiert werden. Dennoch bietet das Verständnis, wie man Basisauthentifizierung in C mit libcurl implementiert, eine grundlegende Fähigkeit, die an verschiedene Authentifizierungsmethoden und Protokolle angepasst werden kann, und spiegelt die nuancierten Abwägungen zwischen Sicherheit, Bequemlichkeit und Anforderungen der Anwendung in der Webentwicklung wider.
