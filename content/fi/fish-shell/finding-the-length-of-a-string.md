---
title:    "Fish Shell: Merkkijonon pituuden löytäminen"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Miksi
Kuinka usein olet törmännyt tarpeeseen selvittää jonkin merkkijonon pituus? Onko sinulla ollut vaikeuksia löytää tehokasta tapaa tehdä se? Kuten monissa muissakin ohjelmointikielissä, myös Fish Shell tarjoaa helpon tavan löytää merkkijonon pituus. Miksi sitten vaivautua miettimään vaikeita ratkaisuja, kun voit käyttää keskeistä Fish Shell -toimintoa tähän tarkoitukseen?

## Kuinka tehdä
Koodiesimerkkien avulla näytämme, kuinka helposti voit käyttää Fish Shellin `string`-toimintoa löytääksesi merkkijonon pituuden.

```
Fish Shell-pohjainen esimerkki:

# Alustetaan merkkijono muuttujaan
set my_string "Tämä on esimerkki"

# Käytetään string-toimintoa löytääksemme merkkijonon pituuden
string length $my_string

# Tulostetaan pituus
echo $status

# Tulostaa: 17
```

Käyttämällä yksinkertaista komentoa `string length` voit helposti löytää minkä tahansa merkkijonon pituuden. Tämä toiminto palauttaa arvon `status`, joka sisältää merkkijonon pituuden numerona. Voit myös tallentaa tämän arvon muuttujaan ja käyttää sitä myöhemmin tarpeen mukaan.

```
Esimerkiksi:
set pituus (string length $my_string)
```

## Syvempi sukellus
`string length` -toiminto toimii myös merkkijonojen lisäksi myös listoilla ja taulukoilla. Se palauttaa näiden tietorakenteiden pituuden lukuna. Lisäksi voit käyttää muita `string`-toiminnon alafunktioita, kuten `string split` ja `string replace`, löytääksesi merkkijonon pituuden haluamallasi tavalla. Lisätietoja näistä löytyy Fish Shellin virallisesta dokumentaatiosta.

## Katso myös
- [Fish Shell string-toiminnon dokumentaatio](https://fishshell.com/docs/current/cmds/string.html)
- [Fish Shell koko ohjeistus](https://fishshell.com/docs/current/)