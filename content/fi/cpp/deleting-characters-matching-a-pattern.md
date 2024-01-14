---
title:                "C++: Mallin mukaisten merkkien poistaminen"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointitilanteissa voi olla tarpeen poistaa merkkejä, jotka vastaavat tiettyä kaavaa. Tämä voi auttaa hallitsemaan ja muokkaamaan tekstiä tai datan käsittelyä tietyissä tapauksissa. Tässä blogipostissa käsittelemme, miten tämä tehdään C++:ssa ja miksi se voi olla hyödyllistä.

## Miten

Alla on esimerkki siitä, miten poistaa merkkejä vastaava kaava käyttämällä C++:n tilapäävariota ja standardikirjastosta löytyviä funktioita. Koodissa poistamme kaikki numerot sisältävät merkit ja tulostamme lopputuloksen:

```C++
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int main() {
    string teksti = "Tämä on esimerkki 123 tekstistä 456!";
    
    // Poistetaan numerot käytettävän kaavan avulla
    teksti.erase(remove_if(teksti.begin(), teksti.end(), ::isdigit), teksti.end());
    
    // Tulostetaan lopputulos
    cout << teksti << endl;
    
    return 0;
}
```

Tulostus:
```
Tämä on esimerkki tekstistä !
```

Tämä esimerkki käyttää `std::remove_if()` funktiota, joka löytyy `<algorithm>` kirjastosta. Tämä funktio ottaa vastaan kolme parametria: alueen aloituselementin, alueen lopetuselementin ja käytettävän ehtofunktion. Ehtofunktio `::isdigit` tarkistaa, onko kyseinen merkki numero vai ei ja poistaa sen, jos se on. Lopuksi `teksti.erase()` funktiolla poistetaan kaikki ehtofunktion palauttamat merkit.

## Syvällisempi tarkastelu

Poistaessa merkkejä vastaavan kaavan C++:ssa on tärkeää muistaa, että poistamisen jälkeen merkkijono ei enää ole samankokoinen kuin ennen. Toisin sanoen, alkuperäistä merkkijonoa ei lyhennetä, vaan poistettavat merkit korvataan lopetuselementin ja alkuelementin välillä. Jos siis haluat täysin lyhentää merkkijonoa, sinun tulee käyttää esimerkiksi `std::erase()` funktiota.

Lisäksi on hyödyllistä tietää, että ehtofunktiota voi muokata haluamallaan tavalla saavuttaakseen halutun lopputuloksen. Esimerkiksi, jos haluat poistaa vain tietyt kirjaimet tai tietyn pituiset merkkijonot, voit muokata ehtofunktiota vastaavasti.

## Katso myös

- [C++ reference - remove_if()](http://www.cplusplus.com/reference/algorithm/remove_if/)
- [C++ reference - erase()](http://www.cplusplus.com/reference/string/string/erase/)
- [C++ reference - isdigit()](http://www.cplusplus.com/reference/cctype/isdigit/)