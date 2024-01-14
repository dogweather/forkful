---
title:                "C: Lähettämällä http-pyyntö perusautentikoinnilla"
simple_title:         "Lähettämällä http-pyyntö perusautentikoinnilla"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi
HTTP-pyyntöjen lähettäminen perusautentikoinnilla on tärkeää, kun haluat kommunikoida verkkosivuston tai sovelluksen kanssa, joka vaatii käyttäjätunnuksen ja salasanan tietojen lähettämistä. Tällainen autentikointi on usein tarpeen esimerkiksi verkkopankkien tai sähköpostipalveluiden kohdalla.

## Miten
Alla on esimerkki koodista, joka lähettää HTTP-pyynnön perusautentikoinnilla ja tulostaa vastauksen.

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://www.example.com/");
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
    curl_easy_setopt(curl, CURLOPT_USERNAME, "käyttäjätunnus");
    curl_easy_setopt(curl, CURLOPT_PASSWORD, "salasana");

    res = curl_easy_perform(curl);
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }
  return 0;
}
```

Tämä koodi käyttää libcurl-kirjastoa lähettämään HTTP-pyynnön osoitteeseen "http://www.example.com/" ja antaa käyttäjätunnuksen ja salasanan HTTP-autentikointiin. Lopuksi se tulostaa vastauksen terminaaliin.

## Syvällinen tarkastelu
HTTP-perusautentikointi toimii lähettämällä käyttäjätunnus ja salasana pyynnön otsikossa "Authorization". Tämä tapahtuu kutsuttaessa CURL-setopt-funktiota ja asettamalla vaihtoehto CURLOPT_HTTPAUTH arvoksi CURLAUTH_BASIC. Tämän jälkeen on myös annettava käyttäjätunnus ja salasana CURLOPT_USERNAME- ja CURLOPT_PASSWORD-vaihtoehtojen avulla.

Libcurl-kirjasto itsessään tarjoaa myös muita mahdollisuuksia HTTP-autentikoinnin hallintaan, kuten esimerkiksi CURLAUTH_DIGEST ja CURLAUTH_NTLM vaihtoehdot.

## Katso myös
- Opas HTTP-autentikoinnista: https://developer.mozilla.org/fi/docs/Web/HTTP/Authentication
- CURL-kirjaston dokumentaatio: https://curl.se/libcurl/c/
- Libcurl-oppaat: https://curl.se/docs/manual.html