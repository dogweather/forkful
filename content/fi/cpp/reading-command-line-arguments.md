---
title:    "C++: Lukeminen komentoriviparametreista"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Miksi

On monia syitä miksi olisi hyödyllistä osata lukea komentoriviargumentteja. Esimerkiksi, kun haluat ohjelman suorittuvan eri tavoin riippuen käyttäjän antamista argumenteista.

## Kuinka

```C++
#include <iostream>

int main(int argc, char** argv) {
    // Jos argumentteja annettiin
    if (argc > 1) {
        // Tulostetaan jokainen argumentti omalle rivilleen
        for (int i = 1; i < argc; i++) {
            std::cout << "Argumentti " << i << ": " << argv[i] << std::endl;
        }
    }
    else {
        // Jos argumentteja ei annettu, tulostetaan viesti
        std::cout << "Et antanut yhtään argumenttia." << std::endl;
    }
    return 0;
}
```

**Lähtö**: Kun suoritat ohjelman ja annat sille argumentteja, saat näkyviin jokaisen argumentin numeron ja sisällön. Jos et anna argumentteja, näet viestin "Et antanut yhtään argumenttia."

## Syvällinen tarkastelu

Kun käynnistät ohjelman komentoriviltä, argv-muuttujassa on tallennettuna kaikki komentoriviargumentit. Tämä muuttuja on tyyppiä "char**", eli taulukko osoittimia merkkijonoihin. Taulukon ensimmäinen alkio (argv[0]) sisältää ohjelman nimen, ja sen jälkeen tulevat argumentit. Tarvittaessa voit käyttää atoi()-funktiota muuttaaksesi merkkijonon kokonaisluvuksi.

## Katso myös

- [Komentoriviargumenttien lukeminen C++:lla](https://www.learncpp.com/cpp-tutorial/122-command-line-arguments/)
- [argv-muuttuja](http://www.cplusplus.com/forum/general/3097/)
- [atoi()](https://www.cplusplus.com/reference/cstdlib/atoi/)