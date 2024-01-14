---
title:                "Gleam: Liitosmerkkijonot"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

# Miksi
Tervetuloa lukemaan blogia Gleam-ohjelmointikielestä! Tämän artikkelin aiheena on "miten liittää merkkijonoja". Vaikka tämä voi kuulostaa yksinkertaiselta tehtävältä, on tärkeää ymmärtää, miksi ja milloin tätä toimintoa voi käyttää.

Käytännössä liittää merkkijonoja yksinkertaisesti tarkoittaa kahden tai useamman merkkijonon yhdistämistä yhdeksi. Tätä voi tarvita esimerkiksi tulostettaessa käyttäjän syöttämää tietoa, kuten nimeä ja sukunimeä, yhtenä merkkijonona.

# Miten
Gleamissa merkkijonojen liittäminen tapahtuu käyttäen plus-merkkiä (`+`). Seuraavassa esimerkissä liitämme merkkijonot "Terve" ja "maailma":

```Gleam 
let tervehdys = "Terve" + "maailma"
```

Kun tulostamme muuttujan `tervehdys` arvon, saamme seuraavan tuloksen:

> Tervemaailma

Huomaa, että Gleamissa aina kun yhdistetään merkkijonoja, tulee niiden välissä olla vähintään yksi tyhjä merkki. Muuten merkkijonot liitetään yhteen ilman välilyöntejä.

# Deep Dive
Gleamissa merkkijonojen liittämisen taustalla on `string` -muuttujan tyyppi. Tämä tarkoittaa, että Gleam käsittelee kaikkia merkkijonoja olioina, joilla on omat sisäiset toiminnallisuudet ja metodeja.

Esimerkiksi voit käyttää `string.concat()` -metodia liittämään useita merkkijonoja yhteen. Voit myös käyttää `string.to_upper()` -metodia muuttamaan merkkijonon kaikki kirjaimet isoiksi kirjaimiksi.

Gleam tarjoaa myös monia muita hyödyllisiä metodeja ja toimintoja merkkijonojen käsittelyyn. Lisätietoja löydät Gleamin virallisesta dokumentaatiosta.

# Katso myös
- [Gleam-ohjelmointikielen virallinen dokumentaatio](https://gleam.run/documentation/)
- [Gleam-ohjelmointikielen perusteet - opetusohjelma](https://gleam.run/tutorials/getting-started/)
- [Miten luoda ja käyttää muuttujia Gleamissa](example.com)