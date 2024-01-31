---
title:                "Verkkosivun lataaminen"
date:                  2024-01-20T17:43:21.931431-07:00
model:                 gpt-4-1106-preview
simple_title:         "Verkkosivun lataaminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Ladataan nettisivu tarkoittaa sivun sisällön hakemista verkosta. Ohjelmoijat tekevät tämän datan käsittelyä, sisällön analysointia tai automaattista toimintaa varten.

## How to: (Kuinka tehdä:)

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
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);
        fclose(fp);
        
        if (CURLE_OK == res) {
            printf("Page downloaded successfully to '%s'.\n", outfilename);
        } else {
            printf("Error: %s\n", curl_easy_strerror(res));
        }
    }
    
    return 0;
}
```

## Deep Dive (Sukellus syvyyksiin):

Historiaa: Web-sivun lataaminen ohjelmallisesti on ollut tarpeen lähes internetin alkuajoista lähtien. Ennen `libcurl`-kirjastoa käytettiin matalan tason verkko-ohjelmointia kuten socketteja.

Vaihtoehtoja: `libcurl` on yksi suosituimmista kirjastoista, mutta muita työkaluja ja kirjastoja löytyy, kuten `wget`, `httpclient` ja `libwww`.

Toteutusyksityiskohtia: `libcurl` tarjoaa monipuolisen API:n useille protokollille kuten HTTP, HTTPS ja FTP. Koodissa `write_data` funktio määrittää, miten ladattu data kirjoitetaan levylle. `CURLOPT_WRITEFUNCTION` ja `CURLOPT_WRITEDATA` curl-optiot ovat tässä keskeisessä roolissa.

## See Also (Katso myös):

- cURL Projektin viralliset sivut: [https://curl.se/](https://curl.se/)
- C-kielen verkkokoodaus: [http://beej.us/guide/bgnet/](http://beej.us/guide/bgnet/)
- Stack Overflow -keskustelut: [https://stackoverflow.com/](https://stackoverflow.com/) (hakusanalla "C programming download webpage")
