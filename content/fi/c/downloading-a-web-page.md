---
title:                "Verkkosivun lataaminen"
html_title:           "C: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Lataaminen on prosessi, jossa tietokone hakee tietoa internetistä ja tallentaa sen paikalliseen laitteeseensa. Ohjelmoija voi ladata verkkosivustoja, taulukoita tai muita tietoja tarpeidensa mukaan. Tämä on tärkeä osa monien ohjelmien toimintaa, kuten verkkoselaimet, suurien tietokantojen hallintaohjelmat ja älypuhelinsovellukset.

## Miten:

Esimerkkeinä käyttäen ```C...``` koodiesimerkkejä, tässä selitetään, kuinka ladataan yksinkertaisen verkkosivun sisältö ja tallennetaan se paikalliseen tiedostoon.

**Esimerkki 1: Lataa ja tulosta verkkosivun sisältö**

```c
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

int main()
{
  // Alustetaan CURL-käsittely
  CURL *curl;
  CURLcode res;

  // Alustetaan muuttujat url-osoitteelle ja latauksen tulostamiseen
  char *url = "https://www.esimerkki.fi";
  FILE *file = fopen("esimerkki.html", "w");

  // Alustetaan CURL:n oletusasetukset
  curl = curl_easy_init();

  if(curl) {
    // Asetetaan URL-osoite
    curl_easy_setopt(curl, CURLOPT_URL, url);
    // Asetetaan kirjoitettava tiedosto
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, file);
    // Asetetaan tulostuksen muoto
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL);
    // Suoritetaan lataus ja tallennus
    res = curl_easy_perform(curl);
    // Suljetaan tiedostokahva
    fclose(file);
    // Suljetaan CURL-käsittely
    curl_easy_cleanup(curl);
    // Tulostetaan lataustulos
    printf("Sivu ladattu ja tallennettu tiedostoon esimerkki.html.\n");
  }

  return 0;
}
```

**Esimerkki 2: Lataa ja tallenna verkkosivun taulukko**

```c
#include <stdio.h>
#include <stdlib.h>
#include <curl/curl.h>

int main()
{
  // Alustetaan CURL-käsittely
  CURL *curl;
  CURLcode res;

  // Alustetaan muuttujat url-osoitteelle ja latauksen tulostamiseen
  char *url = "https://www.esimerkki.fi/taulukko.html";
  FILE *file = fopen("taulukko.csv", "w");

  // Alustetaan CURL:n oletusasetukset
  curl = curl_easy_init();

  if(curl) {
    // Asetetaan URL-osoite
    curl_easy_setopt(curl, CURLOPT_URL, url);
    // Asetetaan kirjoitettava tiedosto
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, file);
    // Asetetaan tulostuksen muoto
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, NULL);
    // Suoritetaan lataus ja tallennus
    res = curl_easy_perform(curl);
    // Suljetaan tiedostokahva
    fclose(file);
    // Suljetaan CURL-käsittely
    curl_easy_cleanup(curl);
    // Tulostetaan lataustulos
    printf("Taulukko ladattu ja tallennettu tiedostoon taulukko.csv.\n");
  }

  return 0;
}
```

## Syväsukellus:

Lataaminen on ollut osa tietokoneiden toimintaa lähes niiden alkuajoista lähtien. Alkuvaiheessa se oli yksinkertaista tekstin tai kuvien latausta puhelinlinjan ylitse modemien avulla. Nykyään lataaminen on tärkeä osa internetin toimintaa, ja se on kehittynyt monipuolisemmaksi erilaisten sisältöjen lataamiseen. Ohjelmoijat käyttävät lataamista lähes kaikissa verkkosovelluksissa ja -palveluissa, joten sen hallitseminen on tärkeää.

On olemassa myös muita keinoja ladata web-sisältöä, kuten käyttämällä Pythonin Requests-kirjastoa tai Node.js:n request-moduulia. Kunkin ohjelmointikielen ja -ympäristön mukaan voi olla hyödyllisempää käyttää tiettyjä työkaluja.

Lataaminen toteutetaan usein käyttämällä protokollaa, kuten HTTP tai FTP, ja sen avulla voidaan myös lisätä turvallisuutta lähettämällä tiedot salattuna.

## Katso myös:

- [C-curl-verkkosivu](https://curl.haxx.se/libcurl/c/) - virallinen dokumentaatio C-kielelle
- [Python Requests-kirjasto](https://requests.readthedocs.io/en/master/) - dokumentaatio ja esimerkkejä Python-kielelle
- [Node.js request-moduuli](https://www.npmjs.com/package/request) - dokumentaatio ja esimerkkejä Node.js:lle