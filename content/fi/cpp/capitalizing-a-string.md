---
title:                "C++: Merkkijonon muuttaminen isoksi kirjoitusmuodoksi."
simple_title:         "Merkkijonon muuttaminen isoksi kirjoitusmuodoksi."
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi: Miksi kannattaa käyttää merkkijonon suurennusta?

Jokaisessa ohjelmointikielessä on erilaisia toimintoja, jotka voivat helpottaa kehittäjän arkea ja tehdä koodista helpommin luettavaa ja ylläpidettävää. Merkkijonon suurennus on yksi tällainen toiminto, joka voi olla hyödyllinen monissa ohjelmointiprojekteissa.

## Miten: Esimerkkejä koodista ja tulosteista

Merkkijonon suurennusta voidaan käyttää muuttamaan merkkijonon ensimmäinen kirjain isoksi kirjaimeksi. Tämä voi olla hyödyllistä esimerkiksi koodissa, jossa käyttäjän syöttämä teksti halutaan muuttaa yhtenäiseen muotoon.

Esimerkiksi seuraavassa koodissa käytämme C++:n capitalize-funktiota muuttamaan käyttäjän antaman merkkijonon ensimmäisen kirjaimen isoksi kirjaimeksi:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string s;
    cout << "Anna merkkijono: ";
    cin >> s;

    s[0] = toupper(s[0]); // muutetaan ensimmäinen kirjain isoksi

    cout << "Suurennettu merkkijono: " << s << endl;

    return 0;
}
```

Kun käyttäjä antaa syötteeksi esimerkiksi "program", saadaan tulostettaessa "Suurennettu merkkijono: Program".

Toinen tapa käyttää capitalizea on käyttää sille annettua merkkijonoa ja tallentaa se uuteen muuttujaan. Tämä on hyödyllistä esimerkiksi silloin, kun halutaan käyttää alkuperäistä merkkijonoa myöhemmin, mutta myös suurennettua versiota.

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string original = "ohjelmointi";
    string capitalized = capitalize(original); // tallennetaan uuteen muuttujaan

    cout << "Alkuperäinen merkkijono: " << original << endl;
    cout << "Suurennettu merkkijono: " << capitalized << endl;

    return 0;
}
```

Tulostettaessa saadaan "Alkuperäinen merkkijono: ohjelmointi" ja "Suurennettu merkkijono: Ohjelmointi".

## Syvällisempi tarkastelu merkkijonon suurennuksesta

Merkkijonon suurennus toimii muuttamalla merkkijonon ensimmäisen kirjaimen isoksi kirjaimeksi ja jättämällä muut kirjaimet ennalleen. Tämä tapahtuu käyttämällä toupper-funktiota, joka muuttaa annetun kirjaimen isoksi.

On kuitenkin hyvä huomata, että toupper-funktio ei tee muutoksia itse merkkijonoon, vaan palauttaa uuden suurennetun merkkijonon. Tämän vuoksi meidän täytyy tallentaa palautettu merkkijono uuteen muuttujaan, mikäli haluamme käyttää alkuperäistä merkkijonoa myöhemmin.

## Katso myös

- [C++:n toupper-funktio](https://www.cplusplus.com/reference/cctype/toupper/)
- [Stringin muokkaaminen C++:ssa](https://www.tutorialspoint.com/cplusplus/cpp_strings_handling.htm)

Kiitos, että luit tämän blogikirjoituksen merkkijonon suurennuksesta C++:ssa! Toivottavasti siitä oli sinulle hyötyä ja se auttaa sinua parantamaan koodisi luettavuutta ja yllä