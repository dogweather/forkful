---
title:                "Verkkosivun lataaminen"
html_title:           "C++: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Web-sivun lataaminen tarkoittaa, että ohjelma noutaa tai hakee tietoa Internetistä ja tallentaa sen käyttöä varten. Ohjelmoijat tarvitsevat tätä toimintoa esimerkiksi verkkosivujen sisällön hakemiseen ja käsittelyyn.

## Miten:

```C++
#include <iostream>
#include <curl/curl.h>

int main() {
    CURL *curl;
    CURLcode res;
    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/");
        res = curl_easy_perform(curl);
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Ulostulo:

```
<!DOCTYPE html>
<html>
    <head>
        <title>Example Domain</title>
        <meta charset="utf-8" />
        <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
    </head>
    
    <body>
        <div>
            <h1>Example Domain</h1>
            <p>This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.</p>
            <p><a href="https://www.iana.org/domains/example">More information...</a></p>
        </div>
    </body>
</html>
```

## Syväsukellus:

Web-sivujen lataamiseen on kehitetty erilaisia tekniikoita ja kirjastoja, joista yksi suosituimmista on libcurl-kirjasto. Se tarjoaa yksinkertaisen ja monipuolisen käyttöliittymän ladata web-sivuja eri protokollien, kuten HTTP, HTTPS ja FTP, kautta.

Vaihtoehtoisesti web-sivun lataamiseen voidaan käyttää myös muunlaisia menetelmiä, kuten REST API -palveluita tai HTML-parsereita, mutta ne voivat olla monimutkaisempia käyttää ja edellyttää enemmän koodausta.

Lisäksi web-sivun lataamisen toteuttaminen voi olla haastavaa, sillä siihen voi liittyä erilaisia ongelmia, kuten virheellisiä yhteyksiä tai hitaita palvelimia. Koodia pitää kirjoittaa huolellisesti ja testata useita eri tietolähteitä, jotta latausprosessi on luotettava ja toimii oikein erilaisissa tilanteissa.

## Katso myös:

- [CURL-kirjaston kotisivut](https://curl.haxx.se/libcurl/)
- [C++ koodiesimerkki web-sivun lataamiseen](https://github.com/curl/curl/blob/master/docs/examples/simple.c)
- [HTML Parser -kirjasto C++:lle](https://github.com/AtrCheema/HTML-Parser-Cpp)