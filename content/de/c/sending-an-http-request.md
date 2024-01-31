---
title:                "Einen HTTP-Request senden"
date:                  2024-01-20T17:59:17.662591-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen HTTP-Request senden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?

HTTP-Anfragen ermöglichen die Kommunikation mit Webservern – es ist, als würdest du mit dem Server plaudern. Programmiere verwenden sie, um Daten zu senden und zu empfangen; das ist essenziell für Web-Interaktionen.

## Wie geht das:

Hier ein simples Beispiel in C, um eine GET-Anfrage zu senden. Vorher `libcurl` installieren:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        // Füge hier noch andere Optionen hinzu, falls nötig
        CURLcode res = curl_easy_perform(curl);

        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        }
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Beispiele der Ausgabe findest du nicht hier – es hängt davon ab, was `http://example.com` zurückgibt.

## Tiefere Einblicke

Früher war HTTP-Kommunikation nur über das korrekte Schreiben von Sockets möglich – ein mühsamer Prozess. Aber dann kam `libcurl`, eine standfeste Bibliothek für solche Aufgaben. 

Alternativen? Klar: `libhttp`, `Poco C++ Libraries` und für die Mutigen, rein manuelle Sockets.

Die Implementierung über `libcurl` ist beliebt, weil es die Komplexität nimmt. Es kümmert sich um URLs, Header, Keep-Alive und mehr. Ohne solche Hilfsmittel musst du dich mit TCP/IP-Stacks und HTTP-Protokollen herumschlagen.

Die Ausführung einer HTTP-Anfrage in C geht eigentlich um drei Schritte:

1. Initialisiere die Bibliothek (hier `libcurl`).
2. Setze die nötigen Optionen (URL, HTTP-Methode, Header, etc.).
3. Führe die Anfrage aus und handle die Antwort.

Jeder Schritt hat potenzielle Fallstricke – Fehlerbehandlung ist ein Muss.

## Weiterführende Quellen

Sieh dir die Dokumente an, die fürs Herz der HTTP-Anfragen in C sind:

- [libcurl Documentation](https://curl.haxx.se/libcurl/c/)
- [HTTP Made Really Easy](https://www.jmarshall.com/easy/http/)
- [Curl Introduction Tutorial](https://curl.se/docs/httpscripting.html)

Diese Links erklären, wie du mit `libcurl` loslegst, zeigen dir die Grundlagen von HTTP und lehren dich fortgeschrittene Skripttechniken mit `curl`.
