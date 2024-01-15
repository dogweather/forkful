---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Rust: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Syy, miksi rust-kielellä tekstin etsiminen ja korvaaminen on hyödyllistä, johtuu suurelta osin koodin ylläpidettävyydestä. Tekstin korvaamisella voidaan nopeasti tehdä massiivisia muutoksia tekstipohjaisessa koodissa, mikä säästää aikaa ja vaivaa koodin kehityksen ja parantelun yhteydessä.

## Miten

Rustilla tekstien etsiminen ja korvaaminen onnistuu helposti käyttäen metodikutsua ```replace```. Tämä kutsu ottaa vastaan kaksi merkkijonoa: etsittävän tekstin ja sen korvaavan tekstin. Esimerkiksi:

```Rust
let teksti = "Tervetuloa, maailma!";
let korvaus = teksti.replace("maailma", "Rust-ohjelmoijat");
println!("{}", korvaus);

//tulostaa "Tervetuloa, Rust-ohjelmoijat!"
```

Huomaa, että ```replace``` palauttaa uuden merkkijonon eikä muuta alkuperäistä merkkijonoa. Tämä varmistaa, että alkuperäinen teksti säilyy muuttumattomana ja toimii hyvin tekstien etsintälogiikan kanssa.

Voit myös rajoittaa, kuinka monessa kohdassa tekstiä korvataan. Tämä tapahtuu antamalla kolmas parametri ```replace```-metodille, joka kertoo korvausten määrän. Esimerkiksi:

```Rust
let teksti = "Hei kaikki, hei sinä, hei heille!";
let korvaus = teksti.replace("hei", "moi", 2);
println!("{}", korvaus);

//tulostaa "Moi kaikki, moi sinä, hei heille!"
```

Jos haluat etsiä ja korvata tekstiä kirjainkoosta huolimatta, voit käyttää ```replace```-metodin sijasta ```replace_all```-metodia. Tämä metodi korvaa kaikki esiintymät, jotka vastaavat annettua merkkijonoa, riippumatta siitä, ovatko ne isoja vai pieniä kirjaimia. Esimerkiksi:

```Rust
let teksti = "Kirjautuminen onnistui!";
let korvaus = teksti.replace_all("ONNISTUI", "epäonnistui");
println!("{}", korvaus);

//tulostaa "Kirjautuminen epäonnistui!"
```

## Syvemmällä

Rust tarjoaa myös muita työkaluja tekstin käsittelyyn, kuten säännöllisiä lausekkeita ja string manipulointiin tarkoitettuja kääntäjäbiblioteekkeja. Nämä voivat olla hyödyllisiä monimutkaisemmissa tekstinkäsittelykäytännöissä.

Rustin löydät myös useita käyttäjän luomia paketteja tekstinetsintään ja korvaamiseen, kuten ```regex``` ja ```strsim```. Nämä paketit tarjoavat lisäominaisuuksia ja monipuolisempia vaihtoehtoja kuin Rustin sisäiset vaihtoehdot.

## Katso myös

- https://doc.rust-lang.org/std/string/struct.String.html#method.replace
- https://doc.rust-lang.org/std/string/struct.String.html#method.replace_all
- https://github.com/rust-lang/regex