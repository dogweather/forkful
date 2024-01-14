---
title:                "C++: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Miksi Vertailla Päivämääriä?

Päivämäärien vertailu on tärkeä osa ohjelmointia, kun halutaan tarkastella aikaa ja päivämääriä eri tilanteissa. Se voi auttaa esimerkiksi tapahtumien järjestämisessä tai päivien välisen eron laskemisessa. Seuraavassa kerromme, miten voit vertailla päivämääriä C++:ssa.

## Miten?

Vertaillaaksesi kahta päivämäärää C++:ssa, sinun tulee käyttää time.h-kirjastoa. Tämän jälkeen voit käyttää "difftime" -funktiota, joka laskee aikavälin kahden päivämäärän välillä. Esimerkiksi katsotaan seuraavaa koodia:

```C++
#include <iostream>
#include <ctime>

int main()
{
  // Määritellään kaksi päivämäärää
  struct tm päivämäärä1 = {0, 0, 0, 10, 3, 2019};
  struct tm päivämäärä2 = {0, 0, 0, 22, 7, 2019};

  // Lasketaan päivämäärien välinen aika
  double aikaväli = difftime(mktime(&päivämäärä2), mktime(&päivämäärä1));

  // Tulostetaan tulos
  std::cout << "Päivien välinen ero on " << aikaväli/ (60 * 60 * 24) << " päivää." << std::endl;

  return 0;
}

```
Tämä ohjelma tulostaa "Päivien välinen ero on 133 päivää." Voit myös lisätä tarvittavia tarkastuksia, kuten päivämäärien oikeellisuuden varmistamiseksi.

## Syventävä Tarkastelu

C++:ssa on myös muita tapoja vertailla päivämääriä. Voit esimerkiksi käyttää <chrono> -kirjastoa, joka tarjoaa enemmän toimintoja ja tarkempaa ajanhallintaa. Tämä kirjasto on kuitenkin uudempi ja vaatii esimerkiksi C++11:n käyttöä.

Päivämäärien vertailun lisäksi voit myös muokata niitä ja tehdä erilaisia laskutoimituksia. Näihin tarvittavat tiedot ja toiminnot löydät tarkemmin C++:n dokumentaatiosta.

## Katso Myös

- [C++ time.h kirjasto](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [C++ chrono kirjasto](https://cplusplus.com/reference/chrono/)
- [C++:n ajanhallinta ja päivämäärien vertailu](https://www.learncpp.com/cpp-tutorial/514-chrono-library-part-2-calculating-duration/)

Kiitos, että luit tämän oppaan päivämäärien vertailusta C++:ssa. Toivottavasti se auttoi sinua ymmärtämään aiheen paremmin ja löytämään itsellesi sopivan menetelmän päivämäärien käsittelyyn. Onnea ohjelmointiin!