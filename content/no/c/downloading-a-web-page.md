---
title:                "Nedlasting av en nettside"
date:                  2024-01-20T17:43:52.920220-07:00
model:                 gpt-4-1106-preview
simple_title:         "Nedlasting av en nettside"

category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å laste ned en nettside betyr å hente HTML og andre ressurser over Internett for å lagre eller behandle lokalt. Programmerere gjør dette for å automatisere datainnhenting, teste nettsider eller skrape innhold.

## Hvordan:
For å laste ned en nettside i C, kan du bruke `libcurl` som et bibliotek for å gjøre HTTP-forespørsler. Her er et enkelt eksempel:

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
    char outfilename[FILENAME_MAX] = "downloaded_page.html";
    
    curl = curl_easy_init();
    if (curl) {
        fp = fopen(outfilename, "wb");
        if (fp == NULL) {
            perror("Kan ikke åpne fil");
            return 1;
        }
        
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        
        res = curl_easy_perform(curl);
        if (res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() feilet: %s\n", curl_easy_strerror(res));
        }
        
        fclose(fp);
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Eksempelutdata:

```
downloaded_page.html lagret.
```

## Dybdeanalyse:
Tilbake på 90-tallet, var det enklere nettsider og få verktøy for å laste dem ned. Programmene `wget` og `curl` er eldre løsninger som fortsatt brukes. Alternativer til C inkluderer scripting i Python med `requests`, men med C får du ytelse og detaljkontroll.

Når du bruker `libcurl` i C, håndterer du direkte nettverkskall og datastrømmer. Det gir deg kraften til å tilpasse nøyaktig hvordan nedlastingen håndteres, feilsøking på et lavt nivå og optimalisere ytelsen.

## Se Også:
* [libcurl Tutorial](https://curl.haxx.se/libcurl/c/)
* [C Standard Library](https://en.cppreference.com/w/c/header)
* [HTTP Made Really Easy](http://www.jmarshall.com/easy/http/) - En grunnleggende forståelse av HTTP.
* [GNU Wget Manual](https://www.gnu.org/software/wget/manual/wget.html) - For sammenligning av kommandolinjealternativer.
