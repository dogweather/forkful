---
title:                "Lähettäminen HTTP-pyyntö perustodennuksella"
date:                  2024-02-03T18:09:23.228746-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lähettäminen HTTP-pyyntö perustodennuksella"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
HTTP-pyyntö perusautentikaatiolla C-kielellä lähettäminen tarkoittaa HTTP-pyynnön muodostamista, joka sisältää Authorization-otsakkeen käyttäjätiedot koodattuna Base64-muotoon. Tämä on yleinen metodi yksinkertaisen autentikaatiokerroksen lisäämiseen HTTP-pyyntöihin, mikä mahdollistaa ohjelmallisesti rajoitettujen resurssien käyttämisen.

## Miten:
Perusautentikaatiolla HTTP-pyynnön lähettämiseksi C-kielellä meidän tulee käyttää libcurl-kirjastoa, joka on suosittu, monikäyttöinen ja helppokäyttöinen asiakaspuolen URL-siirtokirjasto. Se käsittelee erilaisia protokollia, mukaan lukien HTTP ja HTTPS, tekee tehtävämme yksinkertaisemmaksi. Varmista, että libcurl on asennettu järjestelmääsi ennen jatkamista. Tässä on perusesimerkki, joka näyttää, miten lähetetään GET-pyyntö perusautentikaation avulla:

```c
#include <stdio.h>
#include <curl/curl.h>

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        // URL, johon pyyntö lähetetään
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/resource");
        // Perusautentikaation käytön salliminen
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        // Käyttäjätunnuksen ja salasanan antaminen perusautentikaatiolle
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

        // GET-pyynnön suorittaminen
        res = curl_easy_perform(curl);

        // Virheiden tarkistaminen
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n",
                    curl_easy_strerror(res));

        // Aina siivoa
        curl_easy_cleanup(curl);
    }
    
    curl_global_cleanup();

    return 0;
}
```
Yllä olevassa esimerkissä korvaa `"http://example.com/resource"`, `"username"`, ja `"password"` todellisella URL:llä, käyttäjätunnuksellasi ja salasanallasi.

Tämä koodi alustaa `CURL`-objektin, asettaa URL:n, ottaa käyttöön HTTP Basic Authenticationin ja määrittää tunnistetiedot. Sitten se lähettää pyynnön ja siivoaa jälkeensä. Jos se onnistuu, pyydetty resurssi noudetaan; jos ilmenee virhe, se tulostetaan stderr:iin.

Näyteulostus (olettaen, että autentikaatio ja resurssin käyttöoikeus onnistuvat) ei välttämättä näy suoraan ohjelmasta, koska esimerkki osoittaa ensisijaisesti pyynnön lähettämistä. Vastauksen tulostamiseksi sinun tulisi laajentaa ohjelmaa käsittelemään HTTP-vastausdataa.

## Syväsukellus:
HTTP-pyyntöjen lähettäminen perusautentikaatiolla C-kielessä, kuten näytetty, hyödyntää libcurl-kirjastoa sen lujuuden ja yksinkertaisuuden vuoksi. Historiallisesti HTTP-pyyntöjen muodostaminen puhtaasti C-kielellä ilman tällaisia kirjastoja oli hankalaa ja altista virheille, sillä se vaati matalamman tason socket-ohjelmointia ja HTTP-otsikoiden manuaalista koostamista.

Perusautentikaatio itsessään on menetelmä verkon alkuaikoina. Se lähettää tunnistetiedot helposti dekoodattavassa muodossa (Base64), mikä on itsessään turvatonta salaamattomien kanavien yli. Nykyaikaiset sovellukset suosivat usein turvallisempia autentikaatiomenetelmiä, kuten OAuth 2.0:aa tai JWT:tä (JSON Web Tokeneita), erityisesti arkaluonteisen datan kohdalla.

Kuitenkin sisäisissä, vähemmän kriittisissä järjestelmissä tai nopeissa ja likaisissa skripteissä, joissa mukavuus painaa enemmän kuin turvallisuushuolet, perusautentikaatio pysyy käytössä. Lisäksi, kun sitä käytetään yhdessä salattujen yhteyksien (HTTPS) kanssa, sen yksinkertaisuus muodostuu edukseksi nopeaa kehitystä, testausta tai automatisointityötä varten, missä korkeamman tason turvamekanismit eivät ole yhtä välttämättömiä.

Konteksteissa, joissa huippuluokan turvallisuus on ehdoton, vaihtoehtoja, kuten token-pohjainen autentikaatio, tulisi priorisoida. Siitä huolimatta ymmärrys perusautentikaation toteutuksesta C-kielellä libcurlin avulla tarjoaa perustaitoja, joita voidaan mukauttaa erilaisiin autentikaatiomenetelmiin ja protokolliin, heijastaen turvallisuuden, mukavuuden ja sovellusvaatimusten hienovaraisia kompromisseja web-kehityksessä.
