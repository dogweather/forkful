---
title:                "Kirjoittaminen standardivirheelle"
html_title:           "C++: Kirjoittaminen standardivirheelle"
simple_title:         "Kirjoittaminen standardivirheelle"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Tervetuloa takaisin koodauksen pariin! Tänään puhumme siitä, miksi ja miten kirjoittaa standardivirheeseen C++:ssa. Tietenkin voit kirjoittaa tietoja myös standarditulosvirtaan, mutta miksi kirjoittaisit standardivirheeseen?

Standardivirhe on suunniteltu erityisesti virheilmoituksien ja vianmäärityksen tarpeisiin. Se tulostaa kaiken tiedon, joka on lähetetty virheellisesti, mikä auttaa sinua selvittämään ohjelmasi ongelmat.

## Mitä Sinun Tulee Tehdä

Standardivirheeseen kirjoittaminen C++:ssa ei ole vaikeaa. Sinun tarvitsee vain käyttää `std::cerr`-virtaa ja käyttää siihen `<<`-operaattoria, kuten seuraavassa esimerkissä:

```C++
#include <iostream>

int main()
{
    std::cerr << "Tämä on virheilmoitus standardivirheeseen." << std::endl;
    return 0;
}
```

Esimerkissä luodaan yksinkertainen C++-ohjelma, joka tulostaa virheilmoituksen standardivirheeseen käyttämällä `std::cerr`-virtaa. Huomaa, että `std::endl` käytetään rivinvaihtoon.

Jos suoritat tämän ohjelman, saat seuraavan tulosteen:

```
Tämä on virheilmoitus standardivirheeseen.
```

Sieltä löydät kaiken tiedon, joka on kirjoitettu `std::cerr`-virtaan.

## Syvempi Sukellus

Standardivirhe suorittaa muutamia toimintoja taustalla, joihin sinun kannattaa tutustua. Ensinnäkin, se on C++:n `std::ostream`-luokan alaluokka, joka mahdollistaa tiedon kirjoittamisen virtaan `<<`-operaattorin avulla. Toiseksi, se on C++:n tukemien virtaobjektien joukossa, mikä tarkoittaa sitä, että voit käyttää `std::cerr`-virtaa samaan tapaan kuin muita virtaobjekteja, kuten `std::cout`.

On myös tärkeää huomata, että `std::cerr` on yleensä virheiden käsittelemiseen tarkoitettu virta, mutta voit käyttää sitä myös tulostamaan muita tietoja ohjelmasi suorituksen aikana. Voit myös ohjata `std::cerr`-tulosteen toiseen paikkaan, kuten tiedostoon, `std::clog`-virtaan tai jopa `std::cout`-virtaan. Tämä antaa sinulle joustavuutta käyttää sitä monipuolisesti ohjelmasi tarpeiden mukaan.

## Katso Myös

- [C++ Standardikirjasto: ostream-luokka](https://www.cplusplus.com/reference/ostream/ostream/)
- [C++ Standardikirjasto: virtaobjektien joukko](https://www.cplusplus.com/reference/ios/ios/)
- [C++ Standardikirjasto: näkyvyysvirtaobjektien tyyppi](https://www.cplusplus.com/reference/ios/basicios/)