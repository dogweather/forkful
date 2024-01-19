---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Å Laste Ned en Webside i C

## Hva & Hvorfor?
Når man laster ned en webside, henter PCen HTML-koden til siden fra serveren, og lagrer den lokalt. Denne operasjonen er brukt av programmerere for å skrape data, teste nettsider, eller for å ha en offline kopi av innholdet.

## Hvordan Gjøre Det:
Her er en enkel måte å laste ned en webside ved hjelp av biblioteket cURL i C. Det er nødvendig å ha cURL biblioteket installert for å kjøre koden.

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);
    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");

        // Utfør forespørselen, res vil få returkoden
        res = curl_easy_perform(curl);
        
        // Sjekk for feil
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() mislyktes: %s\n",
                    curl_easy_strerror(res));
    
        // Alltid rydd opp
        curl_easy_cleanup(curl);
    }
    curl_global_cleanup();
    return 0;
}
```
## Dypdykk
Henting av en webside har endret seg betydelig siden internett sine spede begynnelse. Fra å hente HTML utelukkende, til nå, med moderne webskraping teknikker, henter vi ikke bare HTML, men også CSS og Javascript. 

Alternativer til cURL inkluderer biblioteker som libcurl i C, og Requests i Python. En implementeringsdetalj å merke seg er at cURL følger HTTP-omdirigeringer som standard, men du kan endre denne oppførselen ved bruk av riktig alternativ i cURL. 

## Se Også 
1. [cURL Official Documentation](https://curl.haxx.se/libcurl/c/)
2. [How the web works - A beginner’s guide to HTTP and HTML](https://www.freecodecamp.org/news/how-the-web-works-a-beginners-guide-to-http-html-db3e7f7961e1/)
3. [A complete tutorial on Web Scraping](https://www.datacamp.com/community/tutorials/web-scraping-using-python)