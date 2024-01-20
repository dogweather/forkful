---
title:                "Skicka en http-förfrågan"
html_title:           "Javascript: Skicka en http-förfrågan"
simple_title:         "Skicka en http-förfrågan"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & varför?

Att skicka en HTTP-förfrågan handlar om att be en server om särskild data, typiskt genom att använda webben. Det är ett grundläggande verktyg för utvecklare att skrapa webbdata, interagera med API:er, eller ändra webbstatus för HTTP-baserade tjänster.

## Hur man gör:

Vi kommer att använda biblioteket libcurl för exempel på HTTP förfrågningar i C. Installera först det här, var noga med att din version har SSL-stöd.

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
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");

    
    #ifdef SKIP_PEER_VERIFICATION
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
    #endif

    res = curl_easy_perform(curl);

    
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    
    
    curl_easy_cleanup(curl);
  }

  curl_global_cleanup();

  return 0;
}
```
Om allt går som det ska kommer programmet att skicka en HTTP GET-förfrågan till "https://example.com" och skriva ut svarskroppen.

## Djupdykning

HTTP-förfrågan är en grundläggande del av arkitekturen i dagens webb, och går tillbaka till de första dagarna av webben på 90-talet. 

Det finns många alternativ till C och libcurl för att skicka HTTP-förfrågan, inklusive Python's requests library, Node.js http library, eller till och med command linje verktyg som curl och wget.

När du jobbar med libcurl, kom ihåg att det under ytan tillåter mer än bara HTTP - det är ett verktyg för många olika protokoll inklusive FTP, SMTP och mer. 

## Se även:
- [libcurl C API dokumentation](https://curl.se/libcurl/c/)
- [HTTP på Wikipedia (engelska)](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)
- [Python's requests library](https://docs.python-requests.org/en/master/)