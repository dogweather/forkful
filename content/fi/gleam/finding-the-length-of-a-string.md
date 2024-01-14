---
title:    "Gleam: Merkkijonon pituuden löytäminen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi sinun kannattaisi selvittää merkkijonon pituus? Yksinkertaisesti siksi, että se on tärkeä osa monia ohjelmointitehtäviä, kuten merkkien laskemista ja tekstianalyysejä.

## Miten

##### Gleamilla on helppo löytää merkkijonon pituus

```Gleam
let string = "Tämä on esimerkki merkkijonosta."
let length = String.length(string)

assert length == 31 // Varmista, että merkkijonon pituus on oikein
```

Tässä esimerkissä aloitamme määrittelemällä muuttujan "string", joka sisältää haluamamme merkkijonon. Sitten käytämme Gleamin funktiota "String.length" selvittääksemme merkkijonon pituuden.
Lopuksi voimme käyttää "assert" -lauseketta varmistaaksemme, että oikea pituus on löydetty.

Voit myös käyttää samaa funktiota suoraan tekstin sisällä, kuten tässä:

```Gleam
let length = String.length("Tämä on esimerkki merkkijonosta.")
```

## Syväluotaus

Funktiomme "String.length" toimii käyttäen UTF-8 -koodausta, joka mahdollistaa merkkien, kuten kirjainten ja emoji-merkkien, käyttämisen merkkijonossa. Se myös käyttää "grapheme clusters" -termiä, joka tarkoittaa merkkien yhdistelmää, joka muodostaa yhden grafeemin kirjautumisjärjestelmässä. Esimerkiksi "ä" voi olla yksi "grapheme cluster" tai kuuluu kahden erillisen merkin yhdistelmään.

Gleamissa on myös muita hyödyllisiä merkkijonofunktioita, kuten "String.is_empty", joka tarkistaa onko merkkijono tyhjä ja "String.reverse", joka kääntää merkkijonon kirjaimet ylösalasin.
Voit tutustua näihin funktioihin Gleamin virallisessa dokumentaatiossa.

## Katso myös

- [Gleamin dokumentaatio](https://gleam.run/docs)
- [Merkkijonojen käsittely Gleamilla](https://gleam.run/articles/strings)

Kiitos lukemisesta ja onnea Gleamin käytössä!