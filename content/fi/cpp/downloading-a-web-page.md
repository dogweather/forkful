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

## Miksi

Monet projektit vaativat kykyä ladata web-sivuja, kuten sisällönhallintajärjestelmiä tai verkkosivujen tarkastusohjelmistoja. Tämän artikkelin avulla voit oppia, kuinka ladata web-sivuja C++:lla ja mitä asioita sinun tulisi ottaa huomioon.

## Kuinka tehdä

Mikä tahansa web-sivu voidaan ladata C++:lla käyttämällä HTTP GET -pyyntöä ja lukemalla vastaus. Tässä esimerkissä käytämme C ++ Biblioteekkia nimeltä "libcurl", joka helpottaa web-sivujen latausta.

```C++ 
#include <iostream>
#include <curl/curl.h>
 
int main(void)
{
    CURL *curl;
    CURLcode res;
 
    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");
        res = curl_easy_perform(curl);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                curl_easy_strerror(res));
 
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Koodin alussa tuomme tarvittavat kirjastot, mukaan lukien "libcurl". Sen jälkeen luomme CURL-objektin ja asetamme sen halutun web-sivun URL-osoitteeseen. Sitten suoritamme pyynnön ja tarkistamme, että kaikki meni hyvin. Lopuksi puhdistamme objektin ja palautamme 0.

## Syvennyksessä

Libcurl tarjoaa lukuisia vaihtoehtoja, joiden avulla voit muokata ja hallita web-sivujen latausta. Esimerkiksi voit asettaa käyttäjätunnuksen ja salasanan, käyttää erilaisia protokollia tai asettaa aikakatkaisun. Voit myös käsitellä vastauksen tarkasti, esimerkiksi katsomalla HTTP-koodia tai lukemalla vastauksen sisältöä.

## Katso myös

- [Libcurlin dokumentaatio](https://curl.haxx.se/libcurl/c)
- [C++:n virallinen sivusto](https://isocpp.org/)
- [HTTP-pyyntöjen käsittely C++:lla](https://curl.haxx.se/libcurl/c/example.html)