---
title:                "C++: Tiedoston lukeminen"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi lukea tekstitiedostoja?

On monia syitä, miksi haluat lukea tekstitiedostoja C++:lla. Tekstitiedostot ovat yleisesti käytettyjä tiedostomuotoja, jotka sisältävät tietoa tekstimuodossa. Esimerkiksi voit tallentaa käyttäjien syöttämät tiedot tekstitiedostoon, jonka avulla voit käsitellä ja analysoida niitä myöhemmin. Tekstitiedostojen lukeminen on myös hyvä tapa oppia tiedostonkäsittelyä C++:lla.

## Kuinka lukea tekstitiedostoja?

Käytännössä tekstitiedostojen lukeminen C++:lla vaatii muutaman vaiheen. Ensinnäkin, sinun tulee avata tiedosto, jonka haluat lukea. Tämän jälkeen voit käyttää virran syöttäjää (input stream) lukeaksesi tiedoston sisällön. Voit sitten käyttää esimerkiksi ```getline()```-funktiota lukeaksesi tiedoston rivit yksi kerrallaan ja tallentaa ne esimerkiksi vektoriin. Lopuksi, muista sulkea tiedosto, kun olet valmis lukemaan sitä.

Tässä on yksinkertainen esimerkki, kuinka lukea tekstitiedosto ja tulostaa sen sisältö konsoliin:

```C++
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using namespace std;

int main() {

    // Avataan tiedosto, tiedoston nimi annetaan parametrina
    ifstream file("tiedosto.txt");

    // Tarkistetaan, että tiedosto avattiin onnistuneesti
    if (file.is_open()) {

        // Luodaan vektori, johon tallennetaan tiedoston rivit
        vector<string> rivit;

        // Luetaan tiedoston rivit ja lisätään ne vektoriin
        string rivi;
        while (getline(file, rivi)) {
            rivit.push_back(rivi);
        }

        // Suljetaan tiedosto
        file.close();

        // Tulostetaan tiedoston sisältö konsoliin
        for (auto rivi : rivit) {
            cout << rivi << endl;
        }

    } else {
        // Tiedoston avaaminen epäonnistui
        cout << "Tiedoston avaaminen epäonnistui." << endl;
    }
    
    return 0;
}

```

Tässä esimerkissä tiedoston sisältö tulostetaan konsoliin, mutta voit käyttää sisältöä haluamallasi tavalla, esimerkiksi tallentaa se muuttujaan tai käsitellä sitä muilla tavoin.

## Syvempi sukellus tekstitiedostojen lukemiseen

C++ tarjoaa monia erilaisia tapoja lukea tekstitiedostoja ja niiden sisältöä. Esimerkiksi ```ifstream```-luokan lisäksi voit käyttää myös ```istringstream```-luokkaa, joka toimii virtana merkkijonolle. Lisäksi voit käyttää erilaisia funktioita, kuten ```seekg()``` ja ```tellg()```, jotka auttavat sijoittamaan lukukursorin tiettyyn kohtaan tiedostossa.

On myös tärkeää huomata, että tekstitiedostojen lukeminen voi sisältää virheiden käsittelyn, esimerkiksi jos tiedosto ei avaudu onnistuneesti tai siitä lukee jotakin virheellistä tietoa. Tämä on tärkeää ottaa huomioon koodatessa.

## Katso myös

- [C++ - ifstream-luokka](https://www.cplusplus.com/reference/fstream/ifstream/)
- [C++ - istringstream-luokka](https://www.cplusplus.com/reference/sstream/istringstream/)