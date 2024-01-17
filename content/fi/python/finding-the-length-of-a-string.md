---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Python: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Merkkijonon pituuden selvittäminen tarkoittaa yksinkertaisesti merkkijonon sisältämien merkkien lukumäärän laskemista. Tämä on tärkeää ohjelmoinnissa esimerkiksi tietojen käsittelyssä ja muokkaamisessa.

## Miten:
```Python
# Esimerkki: Selvitä merkkijonon "Hei maailma" pituus
merkkijono = "Hei maailma"
pituus = len(merkkijono)

print(pituus)

# Output: 11
```

## Syvemmälle:
Merkkijonojen pituuden laskeminen on ollut olennainen osa ohjelmointia jo pitkään. Ennen Pythonin len() -funktion käyttöä, ohjelmoijien täytyi laskea merkkijonon pituus manuaalisesti esimerkiksi käyttämällä for-loopia.

On myös tärkeää huomata, että merkkijonon pituus osoittaa vain merkkien määrän, ei välttämättä merkkien todellista fyysistä pituutta. Tämä johtuu siitä, että eri merkkejä voi esittää eri tavalla muistin käytössä, kuten unicode-merkistössä.

## Katso myös:
- [Pythonin virallinen dokumentaatio: len()-funktio](https://docs.python.org/3/library/functions.html#len) 
- [W3Schools: Python len() -funktio](https://www.w3schools.com/python/ref_func_len.asp)