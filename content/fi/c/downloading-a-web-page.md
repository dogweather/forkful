---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Lataa Webbisivu C:llä

## Mikä & Miksi?

Webbisivun lataus on prosessi, jossa otetaan kopio verkkosivusta laadukkaaseen tallennusmuotoon. Ohjelmoijat tarvitsevat tätä toimintoa datan keräämiseksi, palvelimen tilan tarkistamiseksi tai offline-käyttöä varten.

## Kuinka toimii:

Esimerkkikoodi siitä, kuinka voit ladata web-sivun käyttämällä `libcurl` -kirjastoa:

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

    res = curl_easy_perform(curl);
    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",
              curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }
  return 0;
}
```
Käännä ja aja ohjelma. Näkyvissä tulisi olla `example.com` sivun HTML-koodi.

## Syvällisempi tutkiskelu:

Lataaminen webbisivu on ollut keskeinen osa verkon vuorovaikutusta siitä lähtien, kun HTTP-protokolla otettiin käyttöön 1990-luvun alussa. Kirjastoja, kuten `libcurl` tai `wget`, on kehitetty täyttämään tarve nopeasti ja tehokkaasti.

Vaihtoehtoisia tapoja ladata webbisivun ovat esimerkiksi `wget` tai `httrack`. Mutta `libcurl` on erittäin joustava ja monipuolinen, ja se tukee useita protokollia, mukaan lukien HTTP, HTTPS, FTP.

`libcurl` vastaanottaa datan palvelimelta ja tallentaa sen muistiin. Se pyytää, suorittaa ja hallitsee verkkotoimintoja, jotka johtavat lataukseen. 

## Katso myös:

Libcurl-kirjaston virallinen dokumentaatio on saatavilla [täältä](https://curl.haxx.se/libcurl/c/).

Muita tapoja ladata verkkosivuja voit löytää [täältä](https://stackoverflow.com/questions/1636333/download-and-save-a-file-from-the-web-using-c).