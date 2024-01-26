---
title:                "Hämta en webbsida"
date:                  2024-01-20T17:43:25.438780-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hämta en webbsida"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida innebär att hämta all HTML- och associerad data som en webbläsare skulle för att visa sidan. Programmerare gör detta för att analysera innehållet, testa tillgänglighet eller skrapa data för diverse ändamål.

## Så här gör du:
C har inget inbyggt stöd för nätverksanrop, så vi använder libcurl, ett kraftfullt bibliotek för att hämta data över nätverk. Se till att ha libcurl installerat innan du provar följande kod.

```C
#include <stdio.h>
#include <curl/curl.h>

static size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream) {
    size_t written = fwrite(ptr, size, nmemb, (FILE *)stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "downloaded_page.html";
    
    curl = curl_easy_init();
    if(curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        
        res = curl_easy_perform(curl);
        
        /* Check for errors */ 
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));
        
        /* always cleanup */ 
        curl_easy_cleanup(curl);
        fclose(fp);
    }
    return 0;
}
```

Körs koden, sparas HTML-innehållet i webbsidan example.com till `downloaded_page.html`.

## Fördjupning:
För länge sedan kunde man tanka ner sidor med enkla kommandon som `ftp` eller genom att skriva egna socket-program. idag, tack vare protokoll som HTTP och HTTPS, är det inte så enkelt. 

Med libcurl får vi stöd för dessa protokoll, plus en massa andra funktioner, utan att behöva bekymra oss över låg nivå-nätverksdetaljer. Alternativ till libcurl inkluderar bibliotek som wget lib och httrack, men libcurl anses generellt vara det mest flexibla. 

När det handlar om implementering, tänk på att hålla säkerhetsuppdateringar i åtanke. Webbskrapning och hämtning av data kan också träffa in på juridiska frågor så som upphovsrätt och användaravtal för sajter, så läs på innan du börjar skrapa.

## Se även:
- libcurl officiella webbplats: https://curl.se/libcurl/
- CURL tutorial för C: https://curl.se/libcurl/c/libcurl-tutorial.html
- HTTP protokollet: https://www.rfc-editor.org/info/rfc7230
