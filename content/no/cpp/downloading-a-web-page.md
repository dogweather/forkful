---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å laste ned en webside er en prosess for å hente innholdet fra en spesifikk URL til din lokale maskin. Programmerere gjør dette for å analysere webdata, eller "scrape" informasjon for videre behandling og analyse.

## Hvordan gjør vi det:

Her kommer vi til å bruke `libcurl`, et gratis, klient-side URL overføring bibliotek. Installer `libcurl` før du fortsetter. 

```C++
#include <iostream>
#include <string>
#include <curl/curl.h>

size_t WriteCallback(void* contents, size_t size, size_t nmemb, std::string* userp) {
    userp->append((char*)contents, size * nmemb);
    return size * nmemb;
}

int main() {
    CURL* curl;
    CURLcode res;
    std::string readBuffer;

    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &readBuffer);
        res = curl_easy_perform(curl); 

        if(res != CURLE_OK) {
        fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        } 
      else {
         std::cout << readBuffer << std::endl;
      }

    curl_easy_cleanup(curl);
    }

  curl_global_cleanup();

  return 0;
}
```

Eksempel utdata er innholdet på "http://example.com"

## Dyp Dykk:

Historisk sett er nedlasting av websider vanlig praksis for webcrawling og datascraping. Ved hjelp av avanserte programmeringsspråk, kan utviklere nå skrive script for å automatisk laste ned og behandle data.

Andre metoder for å laste ned sider inkluderer bruk av `wget` eller `httrack`, som er kommandolinjeverktøy for å laste ned hele nettsteder. Deretter har du også bibliotek som `BeautifulSoup` og `Scrapy` i Python.

Når det kommer til implementering, fungerer `libcurl` ved å initialisere en CURL økt, sette nødvendige alternativer (URL, write function, write data) og til slutt utføre økten. Når økten er ferdig, blir den ryddet opp. Denne koden gjør en HTTP GET forespørsel til serveren, som returnerer en respons som er lagret i `readBuffer`.

## Se Også

1. Libcurl dokumentasjon: https://curl.haxx.se/libcurl/c/
2. Tutorial for å laste ned sider med Python og BeautifulSoup: https://realpython.com/beautiful-soup-web-scraper-python/
3. HTTrack website copier: https://www.httrack.com/
4. Wget manual: https://www.gnu.org/software/wget/manual/wget.html