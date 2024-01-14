---
title:                "C++: Aloittaminen uusi projekti"
programming_language: "C++"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Miksi: Aloittaa uusi projekti

Oletko ajatellut aloittaa uuden ohjelmointiprojektin, mutta et tiedä mistä aloittaa? Ei hätää, tämä blogipostaus antaa sinulle vinkkejä ja esimerkkejä siitä, miten aloittaa uusi projekti helposti ja tehokkaasti!

## Miten: Esimerkkejä ohjelmointikoodista

Aloittaminen uuden projektin ei tarvitse olla vaikeaa. Alla on esimerkki siitä, miten voit aloittaa C++-projektin ja tulostaa yksinkertaisen viestin:

```C++
#include <iostream>

int main() {
    std::cout << "Tervetuloa uuteen projektiin!" << std::endl;
    return 0;
}
```

Tämä koodinpätkä tulostaa konsoliin viestin "Tervetuloa uuteen projektiin!". Voit muokata viestiä mieleiseksesi ja lisätä siihen haluamiasi toimintoja.

Entä jos haluat tehdä enemmän kuin vain tulostaa viestin? Esimerkiksi, jos haluat luoda yksinkertaisen laskimen, voit kirjoittaa koodin seuraavasti:

```C++
#include <iostream>

int main() {
    float luku1, luku2;
    char operaattori;
    std::cout << "Syötä ensimmäinen luku: ";
    std::cin >> luku1;
    std::cout << "Syötä toinen luku: ";
    std::cin >> luku2;
    std::cout << "Syötä yhteen-, vähennys-, kerto- tai jakolasku (+, -, *, /): ";
    std::cin >> operaattori;
    if (operaattori == '+') {
        std::cout << luku1 << " + " << luku2 << " = " << luku1 + luku2 << std::endl;
    }
    else if (operaattori == '-') {
        std::cout << luku1 << " - " << luku2 << " = " << luku1 - luku2 << std::endl;
    }
    else if (operaattori == '*') {
        std::cout << luku1 << " * " << luku2 << " = " << luku1 * luku2 << std::endl;
    }
    else if (operaattori == '/') {
        std::cout << luku1 << " / " << luku2 << " = " << luku1 / luku2 << std::endl;
    }
    else {
        std::cout << "Virheellinen operaattori!" << std::endl;
    }
    return 0;
}
```

Tämä koodi luo yksinkertaisen laskimen, joka kysyy käyttäjältä kaksi lukua ja valitsee käyttäjän antaman operaattorin mukaan laskutoimituksen. Voit muokata tätä koodia lisäämällä uusia toimintoja ja laskutoimituksia.

## Syvempi sukellus: Tietoa uuden projektin aloittamisesta

Aloittaessa uuden projektin on tärkeää miettiä, mitä haluat saavuttaa projektiin. Ota huomioon seuraavat asiat:

- Projektin tarkoitus: Mitä haluat saavuttaa projektilla? Onko se vain harjoittelua vai haluatko luoda jotain hyödyllistä?

- Ohjelmointikieli: Päätä, millä ohjelmointikielellä haluat toteuttaa projekti. Voit valita jo osaamasi kielen tai opetella uutta kieltä.

- Ohjelmointiympäristö: Valitse ohjelmointiympäristö, joka sopii parhaiten projektillesi. Esimerkiksi, jos haluat tehdä graafisen käyttöliittymän, voit valita C++:n sijaan Java-kielen ja käyttää esimerkiksi NetBeans-ymp