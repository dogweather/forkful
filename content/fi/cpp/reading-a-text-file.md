---
title:    "C++: Tiedostotekstin lukeminen"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedostojen lukeminen voi olla tarpeellista ohjelmistokehityksessä, sillä usein ohjelmat hyödyntävät tietoa, joka on tallennettu ulkoiseen tiedostoon. Lue lisää, miksi tämä taito on tärkeä ohjelmoijille.

## Miten

Tekstitiedoston lukeminen C++:lla on yksinkertaista käyttämällä stream-kirjastoa. Käytämme `ifstream`-luokkaa, joka mahdollistaa tiedostojen lukemisen. Alla on esimerkki koodista, joka lukee tekstitiedostosta ja tulostaa sen sisällön konsoliin:

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
    // Avataan tekstitiedosto
    ifstream file("tiedosto.txt");
    // Tarkistetaan, että tiedosto avattiin onnistuneesti
    if (!file.is_open()) {
        cout << "Tiedoston avaaminen epäonnistui.";
        return 1;
    }

    // Luetaan tekstitiedoston sisältö ja tulostetaan se konsoliin
    string line;
    while (getline(file, line)) {
        cout << line << endl;
    }

    // Suljetaan tiedosto
    file.close();
    return 0;
}
```

### Tuloste:

```
Tervetuloa lukemaan meidän blogiamme!
Tänään opimme, kuinka luetaan tekstitiedostoja käyttäen C++:aa.
Toivottavasti tämä auttaa sinua tulevissa projekteissasi!
```

## Syväluotaus

Tekstitiedostojen lukeminen on tärkeä taito ohjelmointia opetellessa, sillä se avaa mahdollisuuden käsitellä suurta määrää dataa ulkoisista lähteistä. Tiedostojen lukeminen C++:lla on myös nopeaa ja tehokasta.

Tämä esimerkki käyttää `getline()`-funktiota lukeakseen tiedoston rivit yksi kerrallaan. `getline()` palauttaa jokaisella kutsulla seuraavan rivin, kunnes se saavuttaa tiedoston lopun tai tapahtuu virhe.

On myös mahdollista lukea tiedostoja merkki kerrallaan käyttämällä `get()`-funktiota. Tämä antaa enemmän kontrollia tiedoston lukemiseen, mutta on hieman monimutkaisempi vaihtoehto. Suosittelemme kuitenkin käyttämään `getline()`-funktiota, joka on helpompi ja yleisemmin käytetty vaihtoehto. 

## Katso myös
- [Techtopia: Tiedostojen käsittely C++:lla](https://www.techtopia.com/index.php/C++_kirjaston_fstream_avulla_tehtyjen_tiedostojen_luku)
- [Cplusplus.com: ifstream - tiedostojen lukeminen](http://www.cplusplus.com/reference/fstream/ifstream/)