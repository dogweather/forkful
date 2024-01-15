---
title:                "Http-pyynnön lähettäminen perusautentikoinnilla"
html_title:           "C++: Http-pyynnön lähettäminen perusautentikoinnilla"
simple_title:         "Http-pyynnön lähettäminen perusautentikoinnilla"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyyntöjen lähettämiseen basic authenticationin avulla voi olla monia syitä. Yksi tärkeimmistä on tietoturva. Basic authentication mahdollistaa turvallisen kommunikoinnin web-sovellusten ja palvelinten välillä salakirjoittamalla käyttäjän tunnistetiedot.

## Kuinka

Tässä esimerkissä näytämme, kuinka voit lähettää HTTP-pyynnön basic authenticationin avulla käyttäen C++:ia.

```C++
// Määritä tarvittavat kirjastot
#include <iostream>
#include <curl/curl.h>

using namespace std;

// Alusta muuttujat, jotka sisältävät käyttäjän tiedot
const string USERNAME = "käyttäjätunnus";
const string PASSWORD = "salasana";

// Määritä funktio, joka lähettää HTTP-pyynnön
size_t pyynnon_lahetys_cb(void *contents, size_t size, size_t nmemb, void *userp)
{ 
  return size * nmemb; //Palauttaa vastauksen sisällön koon
}

int main(void)
{
  CURL *curl; // Yhteyden muodostamiseen käytettävä CURL-olio
  CURLcode result; // HTTP-pyynnön lähetyksen tulos tai mahdolliset virheet
  curl = curl_easy_init(); // Alusta CURL-olio

  if(curl) 
  {
    // Aseta CURL-oliolle tarvittavat asetukset
    curl_easy_setopt(curl, CURLOPT_URL, "http://example.com"); // Määritä pyyntöä vastaanottavan palvelimen URL
    curl_easy_setopt(curl, CURLOPT_USERNAME, USERNAME.c_str()); // Aseta käyttäjänimi
    curl_easy_setopt(curl, CURLOPT_PASSWORD, PASSWORD.c_str()); // Aseta salasana
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, pyynnon_lahetys_cb); // Määritä callback-funktio vastauksen käsittelyä varten

    // Lähetä HTTP-pyyntö
    result = curl_easy_perform(curl);

    // Tarkista pyynnön lähetyksen tulos
    if(result != CURLE_OK) 
    {
      cout << "Virhe: " << curl_easy_strerror(result) << endl; // Tulosta mahdollinen virheilmoitus 
    }

    // Pura CURL-olio
    curl_easy_cleanup(curl); 
  }

  return 0;
}
```

**Esimerkkitulostus:**

```
<!doctype html>
<html>
  <head>
    <title>Esimerkkisivu</title>
  </head>
  <body>
    <h1>Tervetuloa!</h1>
    <p>Tervetuloa esimerkkisivulle, käyttäjä tunnistettu!</p>
  </body>
</html>
```

## Deep Dive

Basic authentication toimii lisäämällä `Authorization` -otsake HTTP-pyyntöön. Tämä otsake sisältää käyttäjän tunnistetiedot, jotka on koodattu Base64-formaattiin. Kun vastaanottava palvelin vastaanottaa pyynnön, se purkaa koodauksen ja tarkistaa, ovatko tunnistetiedot oikein. Jos ovat, palvelin antaa pääsyn pyydetylle resurssille.

On tärkeää muistaa, että basic authentication ei tarjoa täydellistä tietoturvaa, sillä tunnistetiedot välitetään edelleen base64-koodattuna. Suositeltavampaa olisi käyttää HTTPS-protokollaa, joka salaa kaiken kommunikoinnin.

## Katso myös

- [CURL](https://curl.se/)
- [Base64-koodaus](https://en.wikipedia.org/wiki/Base64)
- [HTTPS-protokolla](https://en.wikipedia.org/wiki/HTTPS)