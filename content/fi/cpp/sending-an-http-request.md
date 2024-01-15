---
title:                "Lähettää http-pyyntö"
html_title:           "C++: Lähettää http-pyyntö"
simple_title:         "Lähettää http-pyyntö"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyynnön lähettämisellä on monia käyttötarkoituksia, kuten tietojen hakeminen verkkopalvelimelta tai tietojen lähettäminen verkkoon muihin sovelluksiin.

## Miten

Voit lähettää HTTP-pyynnön käyttämällä C++ kielen bibliotekkejä, kuten "cURL" tai "libcurl". Alla on yksinkertainen esimerkki käytöstä ja tulosteesta:

```C++

// Esimerkki GET-pyynnön lähettämisestä

#include <iostream>
#include <curl/curl.h>

int main() {

  // Alusta cURL helppokäyttöisesti

  CURL *curl;
  CURLcode result;

  // Määritä pyyntöosoite

  curl = curl_easy_init();
  curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");

  // Suorita pyyntö ja tulosta vastaus

  result = curl_easy_perform(curl);
  std::cout << "Tuloste: " << result << std::endl;

  // Puhdista resurssit

  curl_easy_cleanup(curl);
  
  return 0;
}

```

Tuloste: 200 (onnistunut vastaus)

## Deep Dive

HTTP-pyyntöjen lähettäminen vaatii yhden tai useamman HTTP-metodin (esim. GET, POST, PUT) käyttämistä sekä osoitteen ja tiedon välittämistä käyttämällä tiettyä protokollaa (HTTP tai HTTPS). Tämä mahdollistaa tietojen siirtämisen verkon kautta yksinkertaisella koodilla.

## Katso myös

- [cURL-kirjaston virallinen dokumentaatio] (https://curl.se/libcurl/c/)
- [HTTP-pyyntöjen lähettäminen C++:lla Stack Overflowssa] (https://stackoverflow.com/questions/19899989/how-to-send-a-http-request-in-c)
- [HTTP-pyyntöjen debuggaaminen Fiddlerin avulla] (https://docs.telerik.com/fiddler/configure-fiddler/tasks/configurefiddlertodetectyourandroiddevice)