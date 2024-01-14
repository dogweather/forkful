---
title:                "C++: Alaohjelmien erottaminen"
simple_title:         "Alaohjelmien erottaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Miksi

Substringien erottaminen on toiminto, joka on välttämätön taito ainakin C++-ohjelmoinnissa. Se antaa sinulle mahdollisuuden ottaa tietyn osan merkkijonosta ja käyttää sitä eri tavoin sovelluksissasi. Tässä blogikirjoituksessa tarkastelemme tarkemmin substringien erottamisen hyötyjä, miten se tehdään ja mitä lisätietoja sinun tulisi tietää tästä tärkeästä ohjelmoinnin osa-alueesta.

# Miten tehdä?

Substringien erottaminen voidaan tehdä useilla eri tavoilla C++-kielisessä koodissa. Yksi yleisimmistä tavoista on käyttää C++:n valmiita funktioita, kuten `substr()`, joka löytyy `<string>`-kirjastosta. Tämä funktio ottaa kaksi parametria: aloitusindeksin ja loppuindeksin ja palauttaa näiden indeksien välissä olevan osan merkkijonosta. Alla on yksinkertainen esimerkki koodista, joka käyttää `substr()`-funktiota:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string teksti = "Tämä on esimerkki tekstistä!";
    string aliteksti = teksti.substr(5, 3); // aloitusindeksi on 5 ja loppuindeksi on 3
    cout << aliteksti << endl; // tulostaa "on "
    return 0;
}
```

Toinen tapa erottaa substring on käyttää `string`-luokan `find()`-funktiota, joka palauttaa parametrina annetun merkkijonon ensimmäisen esiintymäpaikan. Voit sitten käyttää alkuperäisen merkkijonon `substr()`-funktiota saadaksesi haluamasi osan merkkijonosta. Alla on esimerkki koodista, joka käyttää tätä lähestymistapaa:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string teksti = "Tämä on esimerkki tekstistä!";
    size_t indeksi = teksti.find("esimerkki"); // etsii "esimerkki"-merkkijonon indeksin
    string aliteksti = teksti.substr(indeksi, 8); // alkuperäisen merkkijonon ei tarvitse olla tunnettu tällä lähestymistavalla
    cout << aliteksti << endl; // tulostaa "esimerkki"
    return 0;
}
```

On myös mahdollista erottaa substring käyttämällä C++:n `stringstream`-luokkaa, joka muuntaa merkkijonon virtaukseksi ja antaa sinulle mahdollisuuden käyttää erilaisia merkkijonon käsittelytoimintoja. Seuraava esimerkki käyttää `stringstream`-luokkaa saadakseen tekstin ennen ensimmäistä välilyöntiä:

```C++
#include <iostream>
#include <sstream>
#include <string>

using namespace std;

int main() {
    string teksti = "Tämä on esimerkki tekstistä!";
    stringstream virtaus(teksti); // muuntaa tekstin virtaukseksi
    string aliteksti;
    getline(virtaus, aliteksti, ' '); // erottelee merkkijonon ensimmäiseen välilyöntiin asti
    cout << aliteksti << endl; // tulostaa "Tämä"
    return 0;
}
```

# Syvemmälle

On tärkeää huomata, että substringien erottaminen voi olla tehokkain tapa käsitellä merkkijonoja C++:ssa. Tämä johtuu siitä, että se antaa sinulle mahdollisuuden käyttää vain haluamasi osan merkk