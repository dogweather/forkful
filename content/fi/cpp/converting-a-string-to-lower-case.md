---
title:                "C++: Muuntaminen merkkijonosta pienaakkosiin"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi muuntaa merkkijono pienaakkosiin

Merkkijonon muuntaminen pienaakkosiin on usein tärkeä toiminto, jota tarvitaan ohjelmoinnissa erilaisten tekstilähteiden käsittelyssä. Esimerkiksi käyttäjän syöttämä merkkijono voi sisältää niin isoja- kuin pieniaakkosia, ja yhtenäisen muodon varmistaminen on tärkeää monien toimintojen suorittamiseksi oikein.

## Miten muuntaa merkkijono pienaakkosiin

Merkkijonon muuntaminen pienaakkosiin on yksinkertaista käyttämällä "transform" funktiota ja "tolower" metodia. Seuraavassa esimerkissä näytämme, miten tämä voidaan toteuttaa C++ -koodissa.

```C++
#include <iostream>
#include <algorithm>

using namespace std;

int main() {
  string input = "TÄMÄ ON MERKKIJONO PIENAUKKOSINA";
  transform(input.begin(), input.end(), input.begin(), ::tolower);
  cout << input << endl;
  return 0;
}
```

### Output:

`tämä on merkkijono pienaakkosina`

Tässä koodissa käytämme "transform" funktiota käyttäjän syöttämän merkkijonon muuntamiseen. Ensimmäisenä argumenttina annamme merkkijonon alun ja lopun, jotta funktio tietää käsiteltävän osan. Toisena argumenttina annamme sen, mihin haluamme muuntaa merkkijonon ja kolmantena argumenttina annamme "tolower" metodin, joka muuntaa isot kirjaimet pieniksi.

## Syvällinen sukellus merkkijonon muuntamiseen pienaakkosiin

Merkkijonon muuntaminen pienaakkosiin on tärkeä osa merkkijonojen käsittelyä ohjelmoinnissa. Monissa tapauksissa se on tarpeellista, jotta tiedot pysyvät yhtenäisinä ja virheet voidaan välttää. Lisäksi tällä toiminnolla voidaan helposti vertailla merkkijonoja, koska isot ja pienet kirjaimet vaikuttavat merkitykseen ja aakkosjärjestykseen.

On myös huomionarvoista, että tämä toiminto ei muuta alkuperäistä merkkijonoa, vaan se luo uuden kopion, jota voidaan käyttää edelleen. Tämä on tarpeellista, jotta alkuperäinen merkkijono säilyy muuttumattomana ja sitä voidaan edelleen käyttää muissa toiminnoissa.

## Katso myös

- [C++ kirjasto merkkijonojen käsittelyyn](https://www.cplusplus.com/reference/string/)
- [Merkkijonon muuntaminen isoihin ja pieniin kirjaimiin](https://www.geeksforgeeks.org/converting-strings-to-upper-and-lower-case-in-c/)
- [Merkkijonon käsittely C++:ssa](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)