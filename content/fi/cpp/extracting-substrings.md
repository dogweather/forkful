---
title:    "C++: Alirivien erottelu"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Miksi

Miksi sinun kannattaisi aloittaa alimerkkijonojen (substring) muodostaminen C++-ohjelmoinnissa? Alimerkkijonojen muodostaminen on tärkeä taito ohjelmoijalle, sillä se auttaa käsittelemään tekstejä ja merkkijonoja tehokkaammin.

## Miten

C++:ssa on olemassa useita tapoja muodostaa alimerkkijonoja. Yksi tapa on käyttää `substr()`-funktiota. Se ottaa kaksi parametria: aloituskohdan ja halutun alimerkkijonon pituuden. Seuraavassa esimerkissä luodaan alimerkkijono "uva" merkkijonosta "univaate".

```C++
string s = "univaate";
string sub = s.substr(2, 3);
cout << sub << endl;
```

Tässä tapauksessa tulostuu "uva". On myös mahdollista käyttää `getline()`-funktiota, joka ottaa parametrina halutun alimerkkijonon aloituskohdan ja lopetuskohdan.

## Deep Dive

Substrngien muodostaminen voi vaikuttaa yksinkertaiselta, mutta C++:ssa siihen liittyy muutamia seikkoja, jotka on hyvä pitää mielessä. Ensinnäkin, `substr()`-funktio palauttaa kopion alimerkkijonosta, eikä itse merkkijonoon tehdä muutoksia. Tämä tarkoittaa, että jos haluat muokata alimerkkijonoa, sinun tulee tallentaa palautettu alimerkkijono uuteen muuttujaan.

Toiseksi, `getline()`-funktio poimii halutun alueen merkkijonosta ja tallentaa sen uuteen muuttujaan. Jos haluat muuttaa alkuperäisen merkkijonon sisältöä, sinun tulee käyttää `setline()`-funktiota.

## Katso myös

- [C++ - Merkkijonot](https://www.cs.cmu.edu/~pattis/15-1XX/common/handouts/ascii.html)
- [String-säiliön käyttö C++:ssa](https://www.cplusplus.com/reference/string/string/useful_functions/)