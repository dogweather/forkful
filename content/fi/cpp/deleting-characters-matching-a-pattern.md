---
title:                "C++: Kaavan mukaisesti vastaavien merkkien poistaminen"
simple_title:         "Kaavan mukaisesti vastaavien merkkien poistaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmoinnissa on tarpeen poistaa merkkejä merkkijonosta, jotka vastaavat tiettyä kaavaa. Tämä voi johtua esimerkiksi tarpeesta puhdistaa käyttäjän antamaa syötettä tai muokata tietokannasta haettuja tietoja. Tässä blogikirjoituksessa käsittelemme, miten voit poistaa merkkejä, jotka vastaavat tiettyä kaavaa, käyttäen C++ -ohjelmointikieltä.

## Miten se toimii

Käytännön esimerkissä käytämme C++:n sisäänrakennettua `std::string` -luokkaa ja sen `.erase()` -metodia merkkijonon manipulointiin. Alla on esimerkkikoodi, jossa poistamme kaikki numerot merkkijonosta ja tulostamme lopputuloksen.

```C++
#include <iostream>
#include <string>

int main() {
    std::string s = "Tämä on es1imerkki 2 poista3misesta.";

    std::cout << "Alkuperäinen merkkijono: " << s << std::endl;

    // Poistetaan numerot merkkijonosta
    for (int i = 0; i < s.length(); ++i) {
        if (isdigit(s[i])) {
            s.erase(i, 1);
            i--;
        }
    }

    std::cout << "Lopullinen merkkijono: " << s << std::endl;

    return 0;
}

// Output:
// Alkuperäinen merkkijono: Tämä on es1imerkki 2 poista3misesta.
// Lopullinen merkkijono: Tämä on esimerkki poistamisesta.
```

Tässä esimerkissä käytämme `for`-silmukkaa käymään läpi merkkijonon merkkejä. `.length()`-metodi palauttaa merkkijonon pituuden, ja `isdigit()`-funktio tarkistaa, onko kyseessä numero. Jos löydämme numeron, käytämme `.erase()`-metodia poistamaan sen. On tärkeää huomata, että poistettavan numeron indeksin täytyy olla yhden pienempi kuin seuraavan tarkistettavan merkin indeksi, siksi vähennämme `i++` silmukan sisällä.

## Syväsukellus

Vaikka edellä mainittu esimerkki toimii monissa tilanteissa, on hyvä ottaa huomioon muutamia seikkoja, jos haluat poistaa merkkejä merkkijonosta, jotka vastaavat tiettyä kaavaa.

Ensinnäkin, jos tarvitset poistamaan useita erilaisia merkkejä tai kaavoja, on ehkä kätevämpää käyttää `std::regex` -luokkaa ja sen `.regex_replace()` -metodia. Tällä tavalla voit määrittää useita kaavoja yhdellä luettelolla ja korvata ne haluamallasi merkeillä.

Toiseksi, on tärkeää ottaa huomioon, mihin merkkijonon indeksiin olet poistamassa merkkejä. Jos esimerkiksi käytät `while`-silmukkaa ja lisäät sen sisällä yhden merkin jokaiseen poistettavaan kaavaan, seurauksena voi olla loputtomasti toistuva silmukka. Varmista siis, että ohjelmasi pysäyttää silmukan oikeassa kohdassa.

Lisäksi sinun pitäisi harkita tarvitaanko merkkijonon kopio (esimerkiksi `s.erase()` vs. `s.erase()`), jos haluat muuttaa alkuperäistä merkkijonoa tai käyttää sitä myöhemmin.

## Katso myös

- [