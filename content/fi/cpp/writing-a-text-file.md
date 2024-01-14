---
title:                "C++: Tiedoston kirjoittaminen"
simple_title:         "Tiedoston kirjoittaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Miksi

Tekstitiedostojen kirjoittaminen on olennainen osa C++ -ohjelmointia, koska se mahdollistaa tietojen tallentamisen ja lukemisen pysyvästi. Tämä on erityisen hyödyllistä, kun haluat tallentaa käyttäjän syöttämiä tietoja tai luoda tiedostoihin lokitietoja.

# Miten

Tekstitiedoston kirjoittamiseen C++:lla on muutama eri vaihtoehto. Yksi tapa on avata tiedosto ofstream -luokalla ja käyttämällä siihen liittyviä funktioita, kuten open(), close() ja write(). Tässä esimerkissä luodaan tiedosto "teksti.txt" ja kirjoitetaan siihen teksti "Tervetuloa C++:aan!":

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {

    // Avataan tiedosto ja liitetään se ofstream -luokkaan
    ofstream tiedosto("teksti.txt");

    // Tarkistetaan, onko tiedoston avaaminen onnistunut
    if (tiedosto.is_open()) {
        cout << "Tiedosto avattu onnistuneesti!" << endl;

        // Kirjoitetaan teksti tiedostoon käyttämällä write() -funktiota
        tiedosto.write("Tervetuloa C++:aan!", sizeof("Tervetuloa C++:aan!"));
        
        // Suljetaan tiedosto
        tiedosto.close();
    }
    else
        cout << "Tiedoston avaaminen epäonnistui." << endl;

    return 0;
}
```

Tämän koodin suorittamisen jälkeen "teksti.txt" -tiedostoon pitäisi tulla teksti "Tervetuloa C++:aan!".

# Syvällinen tutustuminen

Tekstitiedoston kirjoittamiseen liittyen on hyvä huomioida muutama asia. Ensinnäkin, tiedostoon kirjoitettaessa on tärkeää avata tiedosto ofstream -luokalla, kuten edellisessä esimerkissä teimme. Tämä mahdollistaa tiedoston avaamisen vain kirjoitustarkoitusta varten. 

Toiseksi, tekstiä kirjoitettaessa on huomioitava tiedostoon kirjoitettavien merkkien määrä. Esimerkissä käytimme funktion sizeof() avulla selvittämään annetun tekstinpätkän merkkien määrän. On myös tärkeää huomioida tiedostossa käytetty merkistön koodaustapa (esim. UTF-8 tai ANSI), jotta kirjoitettu teksti näkyy oikein.

Lisäksi, vaikka tiedostoon kirjoittamisessa käytettiin write() -funktiota, voidaan saman tuloksen saavuttaa myös käyttämällä << -operaattoria, kuten perinteisessä C++ -tulostuksessa.

# Katso myös

- [C++ ifstream ja ofstream -luokat (TutorialsPoint)](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [Merkkijonojen koodaukset (w3schools)](https://www.w3schools.com/charsets/default.asp)
- [C++ -virtavirrat ja tiedostojen käsittely (cplusplus.com)](https://www.cplusplus.com/doc/tutorial/files/)