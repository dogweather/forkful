---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:08:37.581063-07:00
description: "Das Senden einer HTTP-Anfrage umfasst das Erstellen und Versenden einer\
  \ Anfrage an einen Webserver, um Daten abzurufen oder zu \xFCbermitteln. Programmierer\u2026"
lastmod: '2024-02-25T18:49:51.399257-07:00'
model: gpt-4-0125-preview
summary: "Das Senden einer HTTP-Anfrage umfasst das Erstellen und Versenden einer\
  \ Anfrage an einen Webserver, um Daten abzurufen oder zu \xFCbermitteln. Programmierer\u2026"
title: Eine HTTP-Anforderung senden
---

{{< edit_this_page >}}

## Was & Warum?

Das Senden einer HTTP-Anfrage umfasst das Erstellen und Versenden einer Anfrage an einen Webserver, um Daten abzurufen oder zu übermitteln. Programmierer tun dies in C, um mit Web-APIs zu interagieren, Webseiten herunterzuladen oder direkt aus ihren Anwendungen heraus mit anderen vernetzten Diensten zu kommunizieren.

## Wie:

Um eine HTTP-Anfrage in C zu senden, stützt man sich in der Regel auf Bibliotheken wie libcurl, da C keine integrierte Unterstützung für Webprotokolle bietet. Hier ist ein einfaches Beispiel, das libcurl verwendet, um eine GET-Anfrage durchzuführen:

Zuerst stellen Sie sicher, dass libcurl auf Ihrem System installiert ist. Danach binden Sie die notwendigen Header ein und verlinken in Ihrer Quelldatei gegen die libcurl-Bibliothek:

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init(); // Initialisieren eines libcurl-Handles
    if(curl) {
        // Festlegen der URL, die das libcurl-Handle erhält
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        // Definieren eines Callbacks, um die Daten zu erhalten
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL); 
        
        // Die Anfrage ausführen, res erhält den Rückgabecode
        res = curl_easy_perform(curl);
        // Auf Fehler überprüfen
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() fehlgeschlagen: %s\n",
                    curl_easy_strerror(res));

        // Immer aufräumen
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Kompilieren Sie dies mit etwas Ähnlichem wie `gcc -o http_request http_request.c -lcurl`, die Ausführung sollte eine einfache GET-Anfrage an "http://example.com" durchführen.

### Ausgabebeispiel

Da das Beispiel die Antwort des Servers nicht verarbeitet, wird die Ausführung keine sichtbare Ausgabe über potenzielle Fehlermeldungen hinaus produzieren. Die Integration der Callback-Funktion zur Verarbeitung empfangener Daten ist für eine sinnvolle Interaktion unerlässlich.

## Vertiefung

Das Konzept, HTTP-Anfragen aus einem C-Programm zu senden, basiert auf den leistungsfähigen Netzwerkfähigkeiten der Sprache, gekoppelt mit externen Bibliotheken, da C selbst eine Low-Level-Sprache ohne integrierten High-Level-Internetprotokollsupport ist. Historisch gesehen würden Programmierer manuell die Socket-Programmierung in C nutzen, ein komplexer und mühsamer Prozess, um mit Webservern zu interagieren, bevor dedizierte Bibliotheken wie libcurl aufkamen.

Libcurl, aufbauend auf C, vereinfacht den Prozess, indem es die mühsamen Details der Socket-Programmierung und die Spezifika des HTTP-Protokolls abstrahiert. Es unterstützt eine Vielzahl von Protokollen über HTTP/HTTPS hinaus, einschließlich FTP, SMTP und mehr, was es zu einem vielseitigen Werkzeug für die Netzwerkprogrammierung in C macht.

Während die Verwendung von libcurl für HTTP-Anfragen in C praktisch ist, tendiert die moderne Programmierung oft zu Sprachen mit integrierter Unterstützung für solche Aufgaben, wie Python (Requests-Bibliothek) oder JavaScript (Fetch API). Diese Alternativen bieten eine einfachere, lesbarere Syntax auf Kosten der granularen Kontrolle und Leistungsoptimierungen, die in C durch direkte Socket-Manipulation und fein abgestimmte Bibliotheksnutzung möglich sind.

Für kritische Leistungsanwendungen oder dort, wo direkte systemnahe Interaktion erforderlich ist, bleibt C eine praktikable Option, insbesondere mit libcurl, das die Komplexitäten der Webkommunikation glättet. Jedoch, für die meisten High-Level-Webinteraktionen könnte die Erkundung spezialisierterer Webprogrammiersprachen effizienter sein.
