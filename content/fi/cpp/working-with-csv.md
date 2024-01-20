---
title:                "Työskentely csv:n kanssa"
html_title:           "C++: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

CSV (Comma-Separated Values) on tiedostomuoto, jota usein käytetään tallentamaan ja jakamaan taulukkomuotoista dataa. Se koostuu riveistä ja sarakkeista, joiden välillä on pilkkuja. Ohjelmoijat käyttävät CSV-tiedostoja, koska se on yksinkertainen tapa jakaa tietoa ja sen lukeminen ja kirjoittaminen on helppoa.

## Miten:

Seuraavassa on esimerkki kuinka lukea CSV-tiedosto ja tulostaa sen sisältö näytölle:

```C++
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>

using namespace std;

int main() {
    ifstream file("tiedosto.csv");
    string rivi;

    while (getline(file, rivi)) {
        vector<string> sarakkeet;
        stringstream ss(rivi);
        string s;

        while (getline(ss, s, ',')) {
            sarakkeet.push_back(s);
        }

        for (int i = 0; i < sarakkeet.size(); i++) {
            cout << sarakkeet[i] << " ";
        }

        cout << endl;
    }

    file.close();
    return 0;
}
```

Esimerkkitiedoston sisältö:

```
nimi, ikä, kaupunki
Matti, 25, Helsinki
Emma, 30, Tampere
```

Ohjelman tulostama tulos:

```
nimi ikä kaupunki
Matti 25 Helsinki
Emma 30 Tampere
```

## Syvällinen Dykkaus:

CSV-tiedoston alkujuuret juontavat 70-luvun lopulle, jolloin tilastotieteilijät alkoivat käyttää sitä tallentaakseen dataa. Nykyään on olemassa muita vaihtoehtoisia tiedostomuotoja, kuten JSON ja XML, mutta CSV on edelleen suosittu sen yksinkertaisuuden vuoksi. CSV-tiedostoja voi lukea ja kirjoittaa monilla ohjelmointikielillä, eikä erillisiä kirjastoja tarvita.

## Katso myös:
