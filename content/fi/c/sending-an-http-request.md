---
title:                "HTTP-pyynnön lähettäminen"
date:                  2024-01-20T17:59:06.996052-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-pyynnön lähettäminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
HTTP-pyynnön lähettäminen on tapa kommunikoida verkossa palvelimen kanssa, jolloin voimme pyytää tietoja tai lähettää niitä. Ohjelmoijat tekevät tämän dataa noutaakseen, lähettääkseen tai API:iden kanssa vuorovaikutuksessa ollessaan.

## How to: (Kuinka tehdä:)
Käytetään libcurl-kirjastoa, mikä on C-kirjasto HTTP-pyyntöjen tekemiseen.

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        /* Lähetetään HTTP GET -pyyntö */
        res = curl_easy_perform(curl);

        /* Tarkistetaan, onnistuiko pyyntö */
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

        /* Siivotaan */
        curl_easy_cleanup(curl);
    }
    return 0;
}
```
Tämän pitäisi tulostaa palvelimen vastauksen terminaaliin tai antaa virheilmoitus, jos pyyntö ei onnistu.

## Deep Dive (Sukellus syvyyksiin):
HTTP-pyyntöjä on lähetetty ohjelmallisesti siitä lähtien, kun web syntyi 1990-luvun alussa. Alkujaan C-ohjelmointikielen avulla toteutettiin yksinkertaisia socket-ohjelmia, jotka kommunikoivat suoraan TCP/IP-protokollan päällä. Libcurl on yksi monipuolisimmista ja suosituimmista kirjastoista HTTP-pyyntöihin, ja se tukee monia eri protokollia, kuten FTP ja SMTP.

Vaihtoehtoja libcurlille voisi olla esimerkiksi POCO C++ Libraries tai Qt Network -moduuli C++:ssa, mutta natiivi C:ssä valinnat ovat rajatumpia. Jotkut ohjelmoijat kirjoittavat matalan tason koodia käyttäen socketeja ja rakentavat HTTP-pyyntöjen käsittelyn itse, mikä antaa täyden kontrollin mutta on työläämpää ja alttiimpaa virheille.

## See Also (Katso myös):
- cURL:n viralliset dokumentit: [https://curl.se/libcurl/c/](https://curl.se/libcurl/c/)
- C socket-ohjelmointi: [Beej's Guide to Network Programming](https://beej.us/guide/bgnet/)
- HTTP-spesifikaatio: [RFC 2616](https://www.ietf.org/rfc/rfc2616.txt)
- Alternative libraries for HTTP in C: [POCO C++ Libraries](https://pocoproject.org/docs/index.html), [Qt Network Documentation](https://doc.qt.io/qt-5/qtnetwork-index.html)
