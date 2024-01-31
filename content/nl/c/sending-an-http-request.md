---
title:                "Een HTTP-verzoek verzenden"
date:                  2024-01-28T22:08:25.942930-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden"

category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het verzenden van een HTTP-verzoek is hoe jouw programma om gegevens vraagt of gegevens naar een webserver stuurt. Programmeurs gebruiken dit om te interageren met API's, webinhoud te grijpen, of te communiceren met andere diensten.

## Hoe:

In C zullen we `libcurl` gebruiken voor deze taak. Het is een krachtige bibliotheek voor gegevensoverdracht met URL's. Eerst moet je `libcurl` installeren. Op Debian-gebaseerde systemen kun je `sudo apt-get install libcurl4-openssl-dev` gebruiken.

Hier is een fragment voor het maken van een eenvoudig GET-verzoek:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        
        /* Voer het verzoek uit, res krijgt de returncode */ 
        res = curl_easy_perform(curl);
        
        /* Controleer op fouten */ 
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() is mislukt: %s\n",
                    curl_easy_strerror(res));
        
        /* altijd opruimen */ 
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Als je dit uitvoert, krijg je geen zichtbare output omdat we de respons niet hebben verwerkt, maar het haalt de inhoud van `http://example.com`.

## Diepe Duik

`libcurl` is gestart in 1997 en is uitgegroeid tot een go-to bibliotheek voor C-programmeurs. Het ondersteunt een groot aantal protocollen, niet alleen HTTP. Alternatieven voor HTTP in C kunnen zijn het schrijven van je eigen implementatie, maar dat is een hobbelige tocht door socketprogrammering en complexe RFC's.

`libcurl` is handig omdat het alle lastige details voor je afhandelt, zoals protocolonderhandeling, foutafhandeling en gegevensoverdracht. Bovendien is het cross-platform - dezelfde code gebruik je op Linux, Windows, Mac, noem maar op.

Onthoud dat `libcurl` standaard een synchrone API gebruikt, wat je hoofdthread kan blokkeren. Als je iets bouwt waar dat toe doet, moet je misschien duiken in multithreading of de `curl_multi_*` set van asynchrone functies.

## Zie Ook

- Officiële libcurl-website voor documentatie en voorbeelden: [https://curl.se/libcurl/](https://curl.se/libcurl/)
- HTTP-protocol details voor achtergrondkennis: [https://www.ietf.org/rfc/rfc2616.txt](https://www.ietf.org/rfc/rfc2616.txt)
- Voor bredere perspectieven op C netwerkprogrammering: [Beej's Guide to Network Programming](https://beej.us/guide/bgnet/)
