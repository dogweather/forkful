---
title:    "Gleam: Merkkijonojen yhdistäminen"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

Miksi: Miksi kannattaa yhdistää merkkijonoja?

Yhdistäminen (concatenation) on yksi perustavanlaatuisimmista ohjelmoinnin käsitteistä, jota tarvitaan lähes kaikissa ohjelmointikielissä. Se tarkoittaa yksinkertaisesti kahden tai useamman merkkijonon yhdistämistä yhdeksi. Vaikka tämä saattaa tuntua yksinkertaiselta, on sillä suuri merkitys monissa ohjelmien rakentamisessa. Seuraavassa kerrotaan, miksi ja miten kannattaa käyttää yhdistämistä Gleam-ohjelmointikielessä.

Kuinka: Esimerkki Gleam-koodilla ja tulostus näytölle

Yksinkertaisin tapa yhdistää merkkijonoja Gleamissa on käyttää plus-merkkijono-operaattoria (+"+). Esimerkiksi, jos haluamme yhdistää merkkijonot "Hei" ja "maailma", voimme kirjoittaa seuraavan koodin:

```Gleam
let teksti = "Hei " + "maailma"
```

Kun suoritamme tämän koodin, saamme tulosteen "Hei maailma". Voimme myös yhdistää useampia merkkijonoja samalla tavalla.

Yhdistämisen avulla voimme myös helposti lisätä muuttujia ja arvoja merkkijonoihin. Esimerkiksi, jos haluamme lisätä muuttujan "nimi" arvon merkkijonoon "Hei", käytämme plus-merkkijono-operaattoria ja merkkiä "#" viestin sisällä:

```Gleam
let nimi = "Matti"
let tervehdys = "Hei #nimi#"
```

Tulostuksen jälkeen tervehdys on "Hei Matti". Tämä tekee koodista helpommin luettavan ja muokattavan, sillä meidän ei tarvitse kirjoittaa kaikkea yhteen pitkään merkkijonoon.

Syvällinen sukellus: Muita tapoja käyttää yhdistämistä

Gleamissa on myös muita tapoja käyttää yhdistämistä, kuten `String.concat` -funktio tai tikunsyötön avulla. Lisäksi, voimme yhdistää merkkijonoja muun muassa `if`-lausekkeiden, `case`-lausekkeiden ja `for`-silmukoiden kanssa. Näitä strategioita kannattaa tutkia lisää, jotta voit hyödyntää yhdistämistä Gleam-ohjelmissasi mahdollisimman tehokkaasti.

Katso myös: Katso seuraavista linkeistä lisää tietoa Gleam-kielen yhdistämisestä ja merkkijonoista:

- Gleam-kielen virallinen dokumentaatio
- Gleam-kurssin 4. osa: Merkkijonot ja yhdistäminen
- String-moduulin dokumentaatio Gleam-kielen GitHub-sivustolla. 

Lopussa on oltava otsikon "Katso myös" ja sitten lista linkejä.