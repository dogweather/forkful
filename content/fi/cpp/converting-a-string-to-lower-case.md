---
title:    "C++: Merkkijonon muuttaminen pieniksi kirjaimiksi"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Jotenkin ei ole ollenkaan harvinaista törmätä tarpeeseen muuttaa merkkijono pieniksi kirjaimiksi. Se voi johtua esimerkiksi vertailusta käyttäjän syöttämän tekstin kanssa tai tulosteen visuaalisesta muotoilusta. Lyhyesti sanottuna, pienten kirjainten käyttö on yleinen ja tarpeellinen ohjelmoinnissa.

## Miten

Merkkijonon muuttaminen pieniksi kirjaimiksi C++: ssa on yksinkertaista. Käytännössä on kaksi yleistä tapaa tehdä tämä. Tässä ensimmäisessä esimerkissä käytetään std::transform-funktiota, joka tarjoaa yksinkertaisen tavan muuttaa merkkijono pienten kirjainten muotoon.

```C++
#include <iostream>
#include <algorithm>
#include <string>

int main() {
    std::string s = "Merkkijono";
    std::transform(s.begin(), s.end(), s.begin(), ::tolower);
    std::cout << "Merkkijonon pienten kirjainten muoto: " << s << std::endl;
}
```

Tuloste: "merkkijono"

Toinen tapa on käyttää kirjastofunktiota tolower(), joka muuttaa yhden merkin pienen kirjaimen muotoon. Käy läpi merkkijonon jokainen merkki ja muuta ne pieniksi kirjaimiksi.

```C++
#include <iostream>
#include <string>

int main() {
    std::string s = "Merkkijono";
    for (int i = 0; i < s.length(); i++) {
        s[i] = tolower(s[i]);
    }
    std::cout << "Merkkijonon pienten kirjainten muoto: " << s << std::endl;
}
```

Tuloste: "merkkijono"

## Syventyminen

Molemmissa esimerkeissä käytetty std::transform()-funktio ja tolower()-funktio ovat osa C++:n standardikirjastoa. Näiden käyttöön ei tarvita ulkopuolisia kirjastoja. Muista kuitenkin tarkistaa, että käytössäsi on oikein määritetty kieliversio, jotta nämä kirjastofunktiot ovat käytettävissä.

On myös hyvä huomata, että merkkijono muutetaan pysyvästi pieniksi kirjaimiksi. Jos haluat vain vertailla alkuperäistä merkkijonoa, voit luoda kopion ja muuttaa sen pieniksi kirjaimiksi.

## Katso myös

- [C++ standardikirjasto: std::transform](https://en.cppreference.com/w/cpp/algorithm/transform)
- [C++ standardikirjasto: tolower](https://en.cppreference.com/w/cpp/string/byte/tolower)