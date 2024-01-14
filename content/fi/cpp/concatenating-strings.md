---
title:    "C++: Merkkijonojen yhdistäminen"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Miksi?

On monia syitä, miksi haluaisimme yhdistää merkkijonoja ohjelmoinnissa. Saattaa olla tarpeen luoda yksi iso merkkijono pienempien palojen pohjalta tai luoda dynaaminen viesti tai nimi käyttäjän antamien tietojen perusteella.

## Miten?


Yhdistäminen eli concatenation tapahtuu yksinkertaisesti yhdistämällä kaksi tai useampia merkkijonoja toisiinsa ja luomalla uusi merkkijono niiden yhdistelmästä. Tätä varten käytämme C++:n operointia '&' merkillä.

Esimerkki:

```C++
#include <iostream>
using namespace std;

int main() {
  string etunimi = "Matti";
  string sukunimi = "Meikäläinen";
  
  string kokonimi = etunimi + " " + sukunimi;
  
  cout << "Tervetuloa, " << kokonimi << "!" << endl;
  return 0;
}
```

Tulostus:

Tervetuloa, Matti Meikäläinen!

## Syvemmälle

Merkkijonojen yhdistämisen lisäksi C++:ssa on muitakin tapoja käsitellä merkkijonoja. Esimerkiksi käyttämällä stringstream kirjastoa, voimme yhdistää muuttujia ja tekstejä helposti ja muuntaa ne merkkijonoiksi.

Toinen hyödyllinen työkalu on to_string funktio, joka muuntaa numerot merkkijonoiksi ja mahdollistaa niiden yhdistämisen muihin merkkijonoihin.

## Katso myös

- [C++ String Tutorial](https://www.w3schools.com/cpp/cpp_strings.asp)
- [C++ stringstream documentation](https://www.cplusplus.com/reference/sstream/stringstream/)
- [C++ to_string function documentation](https://www.cplusplus.com/reference/string/to_string/)