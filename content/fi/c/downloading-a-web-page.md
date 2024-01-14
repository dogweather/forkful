---
title:                "C: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Miksi

Web-sivustojen lataaminen voi olla tärkeä osa C-ohjelman kehittämistä, etenkin jos haluat luoda sovelluksen, joka käyttää dynaamisia sivustoja tai tarvitset tietoa verkkopohjaisista lähteistä. Tämä blogikirjoitus opastaa sinua kuinka voit ladata web-sivuja C-ohjelmalla ja tarjoaa syvällisempää tietoa aiheesta.

# Miten

Web-sivujen lataaminen C-ohjelmalla on suhteellisen yksinkertainen prosessi. Sinun täytyy ensin käyttää librarya, joka sallii verkkoyhteyden luomisen ja lataamisen. Yksi näistä kirjastoista on esimerkiksi libcurl. Seuraavaksi tarvitset koodin, joka määrittää URL-osoitteen, josta haluat ladata sivun. Sitten vain luodaan yhteys ja käsitellään ladattuja tietoja.

Alla on esimerkki koodista, jossa ladotaan C-ohjelman verkkosivu ja tulostetaan sen sisältö:

```C
#include <stdio.h>
#include <curl/curl.h>
 
int main(void)
{
  CURL *curl;
  CURLcode res;
 
  curl = curl_easy_init();
  if(curl) {
    // määritetään URL-osoite
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");
    // lähetetään kaikki tiedot stdout:iin
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, stdout);
 
    // suoritetaan pyyntö
    res = curl_easy_perform(curl);
 
    // vapautetaan muisti ja suljetaan yhteys
    curl_easy_cleanup(curl);
  }
 
  return 0;
}
```

Yllä olevassa koodissa käytetään libcurl-kirjastoa ja sen avulla asetetaan URL-osoite ja määritetään, että sivu lähetetään standard outputiin. Sen jälkeen suoritetaan pyyntö ja vapautetaan käytetyt resurssit. Alla on esimerkki, millainen tuloste koodin ajamisen jälkeen näyttäisi:

```
<!doctype html>
<html>
<head>
<title>Esimerkki</title>
</head>
<body>
<h1>Tervetuloa</h1>
<p>Esimerkki verkkosivusta C-ohjelmassa.</p>
</body>
</html>
```

# Syvemmälle

Web-sivujen lataaminen C-ohjelmalla voi olla monimutkaisempaa, jos haluat käsitellä esimerkiksi erilaisia vastauksia tai virheitä. Voit esimerkiksi käyttää libcurlin tarjoamaa monipuolista dokumentaatiota saadaksesi lisätietoa ja löytääksesi tarvitsemasi toiminnot. Voit myös käyttää muita kirjastoja, kuten libxml, jos haluat analysoida ladattuja HTML-sivuja.

# Katso myös

- [Libcurl dokumentaatio](https://curl.haxx.se/libcurl/)
- [Esimerkkejä C-ohjelmoinnista ja web-latauksesta](https://www.programmersought.com/article/41665870129/)