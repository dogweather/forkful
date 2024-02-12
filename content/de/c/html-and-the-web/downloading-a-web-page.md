---
title:                "Herunterladen einer Webseite"
aliases: - /de/c/downloading-a-web-page.md
date:                  2024-02-03T17:55:41.870104-07:00
model:                 gpt-4-0125-preview
simple_title:         "Herunterladen einer Webseite"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/downloading-a-web-page.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Herunterladen einer Webseite in C beinhaltet den programmatischen Zugriff auf den Inhalt einer Webseite über das Internet und das lokale Speichern zur Verarbeitung oder zur Offline-Nutzung. Programmierer nehmen sich oft dieser Aufgabe an, um Webdienste zu nutzen, Webinhalte zu scrapen oder direkt aus ihren Anwendungen heraus mit Online-Ressourcen zu interagieren.

## Wie geht das:

Um eine Webseite in C herunterzuladen, ist ein beliebter Ansatz die Verwendung der libcurl-Bibliothek, eine effiziente und portable Client-seitige URL-Übertragungsbibliothek. Stellen Sie sicher, dass libcurl in Ihrem Projekt installiert und verlinkt ist. Hier ist ein Beispiel, das zeigt, wie man libcurl verwendet, um den Inhalt einer Webseite herunterzuladen:

```c
#include <stdio.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written = fwrite(ptr, size, nmemb, stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "./downloaded_page.html";

    curl = curl_easy_init(); // Initialisiere eine libcurl Easy-Sitzung
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data); // Callback für das Schreiben empfangener Daten
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp); // Setze den Dateizeiger, um die Daten zu schreiben

        res = curl_easy_perform(curl); // Führe den Dateidownload aus
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() ist fehlgeschlagen: %s\n",
                    curl_easy_strerror(res));
        }

        /* immer aufräumen */
        curl_easy_cleanup(curl); // Räume die Easy-Sitzung auf
        fclose(fp); // Schließe den Dateistrom
    }
    return 0;
}
```
Beispielausgabe (keine sichtbare Ausgabe in der Konsole): Dieser Code lädt den Inhalt der angegebenen URL herunter und speichert ihn in einer Datei namens `downloaded_page.html`. Überprüfen Sie das Verzeichnis Ihres Programms auf diese Datei, um den heruntergeladenen Inhalt zu sehen.

## Tiefergehend:

Historisch gesehen war das Herunterladen von Webinhalten in C umständlicher, da es manuelle Socket-Programmierung und die Handhabung des HTTP-Protokolls erforderte. Libcurl abstrahiert diese Komplexitäten und bietet eine robuste und hochrangige API für den Datentransfer über das Web.

Während libcurl HTTP-Anfragen in C vereinfacht, können moderne Programmiersprachen wie Python mit ihrer `requests` Bibliothek oder JavaScript (Node.js) mit verschiedenen HTTP-Client-Bibliotheken eine intuitivere Syntax und integrierte Unterstützung für JSON und andere Datenformate bieten, die in der Webkommunikation üblich sind. C und libcurl bieten jedoch eine leistungsfähige und stabile Lösung für Systeme, bei denen Effizienz, feingranulare Kontrolle oder die Integration in vorhandene C-Codebasen kritisch sind. Es ist auch erwähnenswert, dass C in Kombination mit libcurl für mehr als nur das Herunterladen von Webseiten verwendet werden kann – es ist fähig zu FTP, SMTP und vielem mehr, was es zu einem vielseitigen Werkzeug im Toolkit eines Programmierers macht.
