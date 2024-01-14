---
title:                "C: Http-pyynnön lähettäminen"
simple_title:         "Http-pyynnön lähettäminen"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit lähettää HTTP-pyynnön? Se voi olla välttämätöntä tietojen lähettämiseksi palvelimelle tai halutessasi tarkistaa tietyn verkkosivun sisällön ilman selaimen käyttöä.

## Miten

Seuraavassa esimerkissä näemme, kuinka lähetät yksinkertaisen GET-pyynnön käyttäen C-kielen kirjastoa "curl". Ensiksi, sisällytetään kirjasto käyttämällä `#include` -komentoa.

```C
#include <curl/curl.h>
```

Nyt, luodaan CURL-osoitin ja alustetaan se käyttämällä `curl_global_init` -funktiota.

```C
CURL *curl;
curl_global_init(CURL_GLOBAL_ALL);
```

Lopuksi, asetetaan URL-osoite ja lähetetään pyyntö käyttäen `curl_easy_perform` -funktiota.

```C
// Asetetaan URL
curl = curl_easy_init();
if(curl) {
  curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");
  // Lähetetään pyyntö
  curl_easy_perform(curl);
  // Lopuksi vapautetaan osoitin
  curl_easy_cleanup(curl);
}
```

## Syvällä

HTTP-pyynnöt ovat osa HyperText Transfer Protocol -protokollaa, jota käytetään siirtämään tietoa verkon yli. Ne sisältävät erilaisia ​​komentoja, kuten GET, POST, PUT ja DELETE, jotka määrittävät pyyntöjen tarkoituksen. Jokaisessa pyynnössä on myös otsikko, joka sisältää tietoa pyynnöstä, kuten URL-osoite ja käytetyt parametrit. Lähettämäsi HTTP-pyynnön tyyppi ja otsikko ovat tärkeitä palvelimen vastausta ajatellen.

## Katso myös

* [CURL-kirjasto](https://curl.haxx.se/libcurl/c/)
* [HTTP-pyynnöt ja vastaukset](https://developer.mozilla.org/fi/docs/Web/HTTP/Overview)
* [GET vs POST -pyynnöt](https://docs.microsoft.com/fi-fi/previous-versions/ee658094(v=vs.85))