---
title:                "Perusautentikaation lähetys http-pyynnön avulla"
html_title:           "C: Perusautentikaation lähetys http-pyynnön avulla"
simple_title:         "Perusautentikaation lähetys http-pyynnön avulla"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Lähettäminen HTTP-pyynnön perusautentikoinnilla on tapa varmistaa, että vain oikeutetut käyttäjät voivat käyttää tiettyä verkkosivustoa tai palvelua. Tämä tapahtuu lähettämällä pyyntö oikean käyttäjätunnuksen ja salasanan kanssa, joka sitten tarkistetaan verkkopalvelimella ennen pyynnön hyväksymistä. Koodin kirjoittajat tekevät tämän suojatakseen herkkiä tietoja ja estääkseen luvattoman pääsyn.

## Kuinka tehdä?
Esimerkiksi, jos haluat lähettää HTTP-pyynnön perusautentikoinnilla käyttäen C-koodia, se näyttäisi tältä:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;
  
  curl = curl_easy_init();
  if (curl) 
  {
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com/");
    curl_easy_setopt(curl, CURLOPT_USERNAME, "kayttajatunnus");
    curl_easy_setopt(curl, CURLOPT_PASSWORD, "salasana");
    res = curl_easy_perform(curl);
    if (res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

Tämä koodi käyttää libcurl-kirjastoa, joka tarjoaa helpon tavan lähettää HTTP-pyyntöjä ja hoitaa autentikointia. Kun koodi suoritetaan, se lähettää pyynnön osoitteeseen "https://www.example.com/" käyttäjätunnuksen ja salasanan kanssa, jotka on asetettu CURLOPT_USERNAME ja CURLOPT_PASSWORD -asetuksissa.

## Syvemmälle:
Autentikointi HTTP-pyynnöissä perustuu RFC 2617 -protokollaan, joka määrittelee Basic Access Authentication -menetelmän. Tämä menetelmä lähettää käyttäjätunnuksen ja salasanan selkeästi, joten se ei ole turvallinen ratkaisu kovin herkille tiedoille. On olemassa myös muita autentikointimenetelmiä, kuten Digest Authentication ja OAuth, jotka tarjoavat paremman tietoturvan.

C-koodin lisäksi on muita tapoja lähettää HTTP-pyyntöjä perusautentikoinnilla, kuten käyttämällä ohjelmistokehyksiä kuten Flask tai Express. Näillä kehyksillä on sisäänrakennettu tuki autentikointiin, joten koodin kirjoittaminen on helpompaa ja tehokkaampaa.

## Katso myös:
- [Curlin virallinen dokumentaatio](https://curl.haxx.se/docs/) sisältää lisätietoja libcurl-kirjaston käytöstä.
- [RFC 2617](https://tools.ietf.org/html/rfc2617) määrittelee Basic Access Authentication -protokollan tarkemmin.
- [Flaskin ohjelmointikirja](https://flask.palletsprojects.com/en/2.0.x/) tarjoaa ohjeita autentikoinnin käyttämisestä Python-kehystä käyttäessä.
- [OAuth 2.0 -protokollan viralliset tiedostot](https://oauth.net/2/) tarjoavat tietoa tämän autentikointimenetelmän käytöstä.