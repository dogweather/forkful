---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "C: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Tiedättekö, mitä tarkoittaa HTTP-pyynnön lähettäminen? Lyhyesti sanottuna se on tapa lähettää pyyntö web-palvelimelle saadaksesi sieltä tietoa, kuten verkkosivun sisällön tai tietokannan tiedot. Tämä on tärkeä taito ohjelmoijien työssä, sillä se mahdollistaa vuorovaikutuksen ja tietojen hakemisen verkosta.

## Kuinka tehdä:
Alla olevassa koodiesimerkissä näytän, miten voit lähettää HTTP-pyynnön C-ohjelmassa. Huomaa, että tässä käytän libcurl-kirjastoa, mutta voit myös käyttää muita vaihtoehtoja, kuten libmicrohttpd tai libevent. Huomaa myös, että tulosteen sisältö voi vaihdella sen mukaan, mihin osoitteeseen pyyntö lähetetään ja millä tavalla vastaus on määritelty.

```C
// Alustetaan libcurl-kirjasto
CURL *curl;
CURLcode res;

// Lähetetään GET-pyyntö ja asetetaan vastausmuoto
curl = curl_easy_init();
if(curl) {
  curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");
  curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);

  // Suoritetaan pyyntö ja tallennetaan vastaus muuttujaan
  res = curl_easy_perform(curl);

  // Tulostetaan vastaus
  if(res == CURLE_OK)
    printf("%s\n", curl_easy_strerror(res));

  // Sammutetaan curl
  curl_easy_cleanup(curl);
}
```

## Syvään sukellus:
HTTP-pyynnöt ovat olleet käytössä jo 30 vuotta ja ovat olennainen osa verkkojen toimintaa. On olemassa muita tapoja lähettää pyyntöjä, kuten REST tai SOAP, mutta HTTP on edelleen yleisin ja yksinkertaisin tapa.

Vaikka libcurl on suosittu valinta HTTP-pyyntöjen lähettämiseen, on myös muita kirjastoja ja työkaluja, kuten libmicrohttpd ja wget. Joissakin tapauksissa saattaa olla järkevämpää käyttää valmiita työkaluja sen sijaan, että rakentaisi oman ratkaisun.

Tarkemmat yksityiskohdat HTTP-pyyntöjen lähettämisestä riippuvat käytetystä kirjastosta ja siitä, mihin tarkoitukseen pyyntö lähetetään. Myös virheiden käsittelyyn kannattaa kiinnittää huomiota, jotta ohjelmasi osaa käsitellä tilanteita, joissa pyyntö ei onnistu.

## Katso myös:
- [Libcurl documentation](https://curl.haxx.se/libcurl/)
- [Libmicrohttpd documentation](https://www.gnu.org/software/libmicrohttpd/)
- [Wget documentation](https://www.gnu.org/software/wget/)