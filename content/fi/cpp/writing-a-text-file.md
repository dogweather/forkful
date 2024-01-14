---
title:    "C++: Tekstitiedoston kirjoittaminen"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstiasioiden lähettäminen on yksi tärkeimmistä tavoista tallentaa ja jakaa tietoa ohjelmoijien keskuudessa. Tekstiasiointi on hyödyllinen myös silloin, kun haluat säilyttää tietoja ohjelman suorituksen aikana tai tulostaa asiakirjan, kuten laskun tai raportin. Tässä blogikirjoituksessa käymme läpi, miten kirjoitat tekstiasiakirjan C++ -ohjelmassa.

## Miten

Tekstiasioiden kirjoittaminen C++:ssa on suhteellisen yksinkertainen prosessi. Ilman paljon puhetta, mennään suoraan koodiin!

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
  // Luo tiedosto
  ofstream tiedosto("tekstiasiakirja.txt");

  // Tarkista, onko tiedosto avattu
  if (tiedosto.is_open()) {
    // Kirjoita teksti tiedostoon
    tiedosto << "Tervetuloa lukemaan blogikirjoitustani!"
             << endl;

    // Sulje tiedosto
    tiedosto.close();
  } else {
    cout << "Tiedoston avaaminen epäonnistui!" << endl;
  }

  return 0;
}
```

Yllä olevassa koodissa luomme uuden tiedoston nimeltä "tekstiasiakirja.txt". Sitten tarkistamme, onko tiedosto avattu onnistuneesti. Jos näin on, kirjoitamme tiedostoon halutun tekstin ja suljemme sen. Muussa tapauksessa tulostamme virheilmoituksen.

Suorittaessamme ohjelman, näemme, että tiedostoon on kirjoitettu teksti "Tervetuloa lukemaan blogikirjoitustani!". Mikäli tarkastelemme tiedostoa tekstieditorilla, näemme sinne tallentuneen saman tekstin.

## Syvällinen sukellus

Tekstiasiakirjan kirjoittaminen C++:ssa vaatii hieman ymmärrystä tiedostojen käsittelystä ja tiedon tallentamisesta. Tiedoston avaaminen voidaan tehdä joko käyttämällä `ofstream` -luokkaa tai `fstream` -luokkaa, joka on yleisempi ja mahdollistaa myös lukemisen tiedostosta.

C++ tarjoaa myös mahdollisuuden asettaa erilaisia väliaikaisia merkkijonojen manipulointi vaihtoehtoja, joita voidaan käyttää tiedoston kirjoittamisessa. Esimerkiksi käyttämällä `setf()` -funktiota, voimme määrittää erilaisia välilyöntejä ja rivinvaihtoja haluamiimme kohtiin tiedostossa.

Tärkeää on myös muistaa sulkea tiedosto suorituksen jälkeen. Tämä estää mahdollisia ongelmia tiedostojen käsittelyssä ja vapauttaa resursseja.

## Katso myös

- [C++ tiedostojen käsittely](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [Tiedostojen kanssa toimiminen C++:ssa](https://www.cs.fsu.edu/~hawkes/cda3101lects/IO.pdf)
- [C++ documentaatio - ofstream luokka](https://en.cppreference.com/w/cpp/io/basic_ofstream)