---
title:                "Lukemassa komentoriviargumentit"
html_title:           "Gleam: Lukemassa komentoriviargumentit"
simple_title:         "Lukemassa komentoriviargumentit"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi sinun kannattaisi oppia lukemaan komentorivin argumentteja Gleam-kielellä. Yksi tärkeimmistä syistä on, että se tekee ohjelmoinnista helpompaa ja tehokkaampaa, koska voit lukea ja käsitellä käyttäjän antamia tietoja suoraan komentoriviltä ilman että joudut käyttämään erillisiä käyttöliittymäkomponentteja.

## Kuinka tehdä se

Komentorivin argumenttien lukeminen Gleam-kielellä on helppoa ja nopeaa. Seuraa näitä yksinkertaisia askeleita saadaksesi komentojasi suoritetuksi oikein ja saadaksesi halutut tulokset.

```Gleam
pub fn main(argv) {
    // Luo vakituinen muuttuja, johon komentorivin argumentit tallennetaan
    let arguments = argv[1]

    // Tulosta argumentit konsoliin
    debug!("#{arguments}")
}
```

Tässä koodiesimerkissä luodaan vakituinen muuttuja nimeltä "argumentit", johon tallennetaan komentorivin annetut argumentit. Tämän jälkeen tulostetaan muuttujan arvo konsoliin käyttäen debug! -funktiota.

```
$> gleam run read_arguments.gleam arg1 arg2
arg1 arg2
```

Syötteestä riippuen, voit luoda tietueita tai suorittaa muita toimintoja argumenttien perusteella. Muista aina tarkistaa ja validoida käyttäjän antamat tiedot ennen kuin käytät niitä ohjelmassasi.

## Syvällisempi sukellus

Lukemalla komentorivin argumentteja, voit myös määrittää niitä erilaisten tyyppien avulla, kuten merkkijonoja tai desimaalilukuja. Voit myös tehdä erilaisia toimintoja argumenttien kanssa, kuten yhdistää niitä, vähentää niitä tai jopa luoda tietueita, joita käytetään ohjelman suorittamisessa.

## Katso myös

- [Gleamin dokumentaatio komentorivin argumenttien lukemisesta](https://gleam.run/articles/command-line-arguments)
- [Tietoa Gleamista ja sen ominaisuuksista](https://gleam.run/)