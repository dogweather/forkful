---
title:                "C++: Väliaikaisen tiedoston luominen"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Tilapäisten tiedostojen luominen on olennainen osa C++ ohjelmointia monissa tapauksissa. Ne antavat ohjelmalle mahdollisuuden tallentaa väliaikaista dataa, kuten käyttäjän syötteitä tai väliaikaisia tuloksia laskutoimituksille, jotka eivät ole osa varsinaista ohjelman toimintaa. Tilapäisten tiedostojen käyttö voi myös olla hyödyllistä testauksessa ja virheenkorjauksessa.

## Kuinka

Tilapäisten tiedostojen luominen C++:ssa on melko yksinkertaista. Tarvitset vain `fstream` kirjaston, joka tarjoaa toimintoja tiedostojen luomiseen ja hallintaan.

```
#include <iostream>
#include <fstream>

int main(){
    // Luo uusi tiedosto ja avaa se kirjoittamista varten
    std::ofstream tiedosto("tilapainen.txt");
    
    // Kirjoita tiedostoon jotain dataa
    tiedosto << "Tämä on tilapäinen tiedosto!" << std::endl;
    
    // Sulje tiedosto
    tiedosto.close();

    return 0;
}
```

Yllä oleva esimerkki luo uuden tiedoston nimeltä "tilapainen.txt" ja kirjoittaa siihen merkkijonon "Tämä on tilapäinen tiedosto!". Tärkeää on sulkea tiedosto `close()` funktion avulla, jotta tiedosto ei jää avoimeksi ja ohjelma ei aiheuta virheitä.

## Syvällinen sukellus

Tilapäisten tiedostojen luominen perustuu C++:n oletuksena tarjoamiin `in` ja `out` virtauksiin, joita voit käyttää `fstream` kirjastossa. Käyttämällä `ofstream` virrata työkaluna voimme määrittää, haluammeko luoda tiedoston uutta dataa varten (`ofstream`) vai lukea olemassa olevaa dataa (`ifstream`). Tämä riippuu siitä, millaista dataa tarvitsemme tilapäiseen tiedostoon.

Tilapäisten tiedostojen luominen on myös hyvä tapa suojata tietoja, jotka on kirjoitettu niiden sisällä, sillä ne poistetaan automaattisesti, kun suoritus päättyy.

## Katso myös

- [C++ fstream - dokumentaatio](https://www.cplusplus.com/reference/fstream/)
- [Tilapäisten tiedostojen luominen C++:ssa](http://www.cplusplus.com/doc/tutorial/files/)