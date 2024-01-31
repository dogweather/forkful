---
title:                "Webseite herunterladen"
date:                  2024-01-20T17:43:32.230273-07:00
model:                 gpt-4-1106-preview
simple_title:         "Webseite herunterladen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Herunterladen einer Webseite bedeutet, die Daten einer Seite über das Internet abzurufen, um sie lokal zu verarbeiten oder zu speichern. Programmierer tun das, um Inhalte zu analysieren, zu archivieren oder zu verarbeiten.

## Vorgehensweise:
```C
#include <stdio.h>
#include <stdlib.h>
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
    char outfilename[FILENAME_MAX] = "downloaded_page.html";
    
    curl = curl_easy_init();
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);
        fclose(fp);
    }
    
    return 0;
}
```
`> download successful (file downloaded_page.html created)`

## Tiefgang:
Früher nutzte man für solche Aufgaben oft `libwww`, doch heute ist `libcurl` beliebter, da es benutzerfreundlicher und vielseitiger ist. `libcurl` unterstützt verschiedene Protokolle wie HTTP, HTTPS, FTP und bietet zahlreiche Optionen für die Authentifizierung, Verbindungseinstellungen und weitere Netzwerkoperationen.

Alternativen zu `libcurl` beinhalten Sprachen höheren Niveaus wie Python mit `requests` oder `BeautifulSoup`, welche das Herunterladen und Parsen von Webinhalten vereinfachen.

Die Implementierung mittels `libcurl` in C erfordert eine gute Fehlerbehandlung und Kontrolle über Netzwerk-Timeouts, um robuste Anwendungen zu schaffen.

## Siehe Auch:
- `libcurl` Dokumentation: https://curl.haxx.se/libcurl/c/
- HTTP-Protokollspezifikation: https://www.w3.org/Protocols/
- 'libwww': https://www.w3.org/Library/ 
- 'wget' Kommandozeilen-Tool: https://www.gnu.org/software/wget/
