---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
HTTP-pyynnön lähettäminen perusautentikoinnilla on prosessi, jossa lähetetään pyyntö verkkopalvelimelle käyttäjätunnusten avulla. Ohjelmistokehittäjät tekevät tämän yleensä käyttäjän varmentamiseksi tai API-rajapintojen käyttämiseksi.

## Miten se tehdään:
Lähetämme HTTP-pyynnön perusautentikoinnilla C++ -koodin avulla käyttämällä esimerkiksi `cURL` kirjastoa. Komennolla `cURL` voit lähettää pyyntöjä verkkopalvelimelle. `cURL` on kirjasto, jota voidaan käyttää erilaisten protokollien kuten HTTP, HTTPS, FTP ynnä muut kautta.

```C++
#include <string>
#include <cstdlib>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);
  curl = curl_easy_init();

  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

    // basic authentication
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
    curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
    curl_easy_cleanup(curl);
  }
  curl_global_cleanup();
  return 0;
}
```
Tuo koodilohko lähettää HTTP GET-pyynnön `example.com`-palvelimeen käyttäjänimen `username` ja salasanan `password` avulla.

## Syventävä sukellus:
HTTP-perusautentikointi on yksinkertainen mekanismi, joka välittää käyttäjätunnuksen ja salasanan Base64-koodattuna. Tämän mekanismin kehitti MIT ja se on osa HTTP/1.0 -standardia vuodelta 1996.

Vaihtoehtoiset menetelmät perusautentikoinnille ovat esimerkiksi kehittyneemmät ja turvallisemmat Digest Access Authentication, OAuth, ja Bearer token authentications. Niissä käytetään salauksia ja todennuskoodeja tietoturvan parantamiseksi.

HTTP-perusautentikointi on kuitenkin yksinkertainen ja helppo implementoida, mutta huomaa, että salasana lähetetään suojattomasti, joten sitä ei pitäisi käyttää ilman SSL/TLS-salausta.

## Katso myös:
Seuraavat linkit ovat hyödyllisiä oppimateriaaleja ja lisätietoja.
1. cURL-kirjaston ohjeet: https://curl.haxx.se/libcurl/c/
2. HTTP-autentikoinnin menetelmiä koskevat tiedot: https://tools.ietf.org/html/rfc2617
3. OpenSSL-kirjaston ohjeet: https://www.openssl.org/docs/