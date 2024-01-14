---
title:    "Gleam: Merkkijonon pituuden löytäminen"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi selvittää merkkijonon pituuden? Merkkijonon pituuden löytäminen on tärkeä osa monien ohjelmien toimintaa. Se auttaa meitä muokkaamaan ja käsittelemään tekstiä, esimerkiksi jos haluamme varmistaa, että käyttäjän antama salasana on tarpeeksi pitkä.

## Miten

Käyttämällä Gleam-ohjelmointikieltä on helppoa löytää merkkijonon pituus. Voit tehdä sen helposti seuraavien esimerkkien ja koodilohkojen avulla.

```Gleam
salasana = "salainen"
pituus = String.length(salasana)
```

Tässä ensimmäisessä rivissä luomme muuttujan, johon tallennamme merkkijonon "salainen". Sitten käytämme String.length-funktiota, joka laskee merkkijonon pituuden ja tallentaa sen muuttujaan "pituus". Voit kokeilla tätä itse ja näet, että pituus on 8.

```Gleam
nimi = "Matti"
pituus = String.length(nimi)
```

Tässä toisessa esimerkissä meillä on lyhyempi merkkijono, "Matti". Kun käytämme taas String.length-funktiota, pituus on nyt 5. Huomaatko, kuinka tärkeää on tarkistaa merkkijonon pituus, jotta voimme varmistaa, että se täyttää tarvittavat vaatimukset?

## Syvä sukellus

Gleam-ohjelmointikielen String-moduuli tarjoaa meille useita hyödyllisiä toimintoja merkkijonojen käsittelyyn. String.length on yksi näistä. Se käy läpi merkkijonon ja laskee kaikki merkit, jotka se sisältää. Tämä on hyödyllinen toiminto, jos haluatte esimerkiksi varmistaa, että käyttäjän antama nimi tai salasana on tarpeeksi pitkä tai haluatte lukea tiedostosta merkkijonoja ja tarkistaa niiden pituuden.

Lisäksi Gleam tarjoaa myös muita hyödyllisiä funktioita, kuten String.concat, joka yhdistää kaksi tai useampaa merkkijonoa yhdeksi, ja String.slice, joka jakaa merkkijonon osiin haluamallamme tavalla. Näiden toimintojen avulla voimme käsitellä ja manipuloida merkkijonoja tehokkaasti.

## Katso myös

- [Gleam-kielen dokumentaatio String-moduulista](https://gleam.run/documentation/stdlib/string/)
- [Gleam-kurssi: Merkkijonot](https://gleam.run/courses/string/) (englanniksi)
- [Merkkijonojen käsittely Gleamilla -artikkeli](https://medium.com/@josemendieta/coding-with-gleam-processing-strings-dd33da2b28cb) (englanniksi)