---
title:                "C++: Http-pyynnön lähettäminen"
simple_title:         "Http-pyynnön lähettäminen"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi lähettää HTTP-pyyntö?

HTTP-pyyntöjen lähettäminen on tärkeä osa modernia web-kehitystä. Se mahdollistaa tietojen lähettämisen ja vastaanottamisen web-sovellusten välillä. Esimerkiksi, jos haluat hakea tietoa API:sta tai ladata kuvia verkkosivulta, sinun täytyy lähettää HTTP-pyyntö.

## Näin teet sen:

```C++
#include <iostream>
#include <curl/curl.h>

int main()
{
    // alustetaan curl
    CURL *curl = curl_easy_init();
    // asetetaan URL-osoite
    curl_easy_setopt(curl, CURLOPT_URL, "http://www.example.com");
    // suoritetaan HTTP-pyyntö
    CURLcode res = curl_easy_perform(curl);
    // tarkistetaan vastauksen statuskoodi
    long response_code;
    curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
    // tulostetaan vastauksen statuskoodi
    std::cout << "HTTP-pyyntö palautti koodin: " << response_code;
    // suljetaan curl
    curl_easy_cleanup(curl);
    
    return 0;
}
```

```
Tulostus: HTTP-pyyntö palautti koodin: 200
```

## Syvemmälle aiheeseen:

HTTP-pyyntöjen lähettäminen voi olla monimutkaista, jos haluat esimerkiksi lähettää erilaisia parametrejä pyyntöön tai hallita vastauksen datan käsittelyä. Onneksi on olemassa laaja valikoima kirjastoja, kuten cURL, jotka voivat auttaa hallitsemaan tätä prosessia.

On myös tärkeää ymmärtää HTTP-protokollan toimintaa ja erilaisia pyyntömetodeja, kuten GET, POST ja PUT. Näiden ymmärtäminen auttaa sinua luomaan tehokkaampia ja turvallisempia web-sovelluksia.

## Katso myös:

- [cURL kirjasto](https://curl.se/libcurl/)
- [HTTP-protokolla](https://developer.mozilla.org/fi/docs/Web/HTTP/Overview)