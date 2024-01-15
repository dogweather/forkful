---
title:                "Lähettämällä http-pyyntö"
html_title:           "C: Lähettämällä http-pyyntö"
simple_title:         "Lähettämällä http-pyyntö"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyyntöjen lähettäminen on tärkeä osa verkkokehitystä, sillä se mahdollistaa kommunikaation verkkopalvelimien ja asiakaslaitteiden välillä. Tämän avulla voidaan esimerkiksi ladata sivuja ja tiedostoja, lähettää lomakkeita ja suorittaa muita toimintoja verkkopalvelimella.

## Kuinka

```C
//Esimerkki GET-pyynnöstä
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  //alustetaan CURL-objekti
  curl = curl_easy_init();
  if(curl) {

    //asetetaan pyyntöosoite
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.esimerkki.com");

    //suoritetaan GET-pyyntö
    res = curl_easy_perform(curl);

    //tarkistetaan, onko pyyntö onnistunut
    if(res != CURLE_OK) {
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    }

    //suljetaan CURL-objekti
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

Esimerkissä käytetään CURL-kirjastoa lähettämään GET-pyyntö verkkosivulle. Ensin luodaan CURL-objekti ja asetetaan sille pyyntöosoite. Sitten suoritetaan pyyntö ja tarkistetaan, onko se onnistunut. lopuksi suljetaan CURL-objekti.

```C
//Esimerkki POST-pyynnöstä
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  //alustetaan CURL-objekti
  curl = curl_easy_init();
  if(curl) {

    //asetetaan pyyntöosoite
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.esimerkki.com/submit_form");
    
    //luodaan POST-data
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, "username=testi&password=12345");

    //suoritetaan POST-pyyntö
    res = curl_easy_perform(curl);

    //tarkistetaan, onko pyyntö onnistunut
    if(res != CURLE_OK) {
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    }

    //suljetaan CURL-objekti
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

Toisessa esimerkissä nähdään, kuinka POST-pyyntö lähetetään verkkosivulle. Tässä tapauksessa myös data lähetetään pyynnön mukana, ja sitä voidaan muokata tarpeen mukaan.

## Syväsukellus

HTTP-pyyntö koostuu useista eri osista, kuten otsakkeista ja pyyntötiedoista. Näitä voidaan asettaa ja muokata CURL-kirjaston avulla käyttäjän tarpeiden mukaan. Tarkempi kuvaus pyynnön rakenteesta löytyy esimerkiksi täältä: https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview.

## Katso myös

- CURL-kirjaston dokumentaatio: https://curl.haxx.se/libcurl/c/
- MDN-verkkosivuston selitys HTTP:stä: https://developer.mozilla.org/en-US/docs/Web/HTTP