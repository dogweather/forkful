---
title:                "Rust: Alimerkkijonojen palauttaminen"
simple_title:         "Alimerkkijonojen palauttaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmoinnissa joudumme työskentelemään merkkijonojen kanssa, ja näiden merkkijonojen sisältöä täytyy ehkä muokata tai etsiä tiettyjä osia. Yksi tapa käsitellä merkkijonoja on erottaa niistä osia eli suoraan sanottuna ottaa niistä osia itsenäisiksi merkkijonoiksi.

## Miten

Rust-ohjelmointikielissä on helppo ja tehokas tapa ottaa merkkijonoista alamerkkijonoja. Käytämme tämän saavuttamiseen `substring`-funktiota. Esimerkiksi, jos haluamme ottaa merkkijonosta `"Tervetuloa"` osan `"velu"`, käyttäisimme seuraavaa koodia:

```Rust
let merkkijono = "Tervetuloa";
let osa = merkkijono.substring(3, 6);
```

Tämä koodi luo uuden merkkijonon `"velu"` ja tallentaa sen muuttujaan `osa`. Asetimme `substring`-funktiolle kaksi parametria: ensimmäinen on aloitusindeksi, joka määrittää mistä kohtaa osa otetaan, ja toinen on lopetusindeksi, joka määrittää mihin kohtaan asti osa otetaan. Muista, että indeksit alkavat aina nollasta, joten `substring(3, 6)` tarkoittaa, että haluamme ottaa osan merkkijonon alkaen neljännestä merkistä ja lopettaen kuudenteen merkkiin. Voit myös käyttää negatiivisia indeksejä, jolloin merkkijonon loppupäästä voidaan ottaa osia.

## Syvemmälle

`substring`-funktiolla on myös muita parametreja, jotka voivat olla hyödyllisiä erilaisissa tilanteissa. Voit esimerkiksi käyttää `substring`-funktiota kolmen parametrin kanssa, jolloin kolmas parametri määrää, kuinka monta merkkiä otetaan osaksi. Tämä on kätevää, jos tiedät tarkalleen kuinka monta merkkiä haluat ottaa osaksi. Voit myös käyttää `substring`-funktiota yhdessä `slice`-funktion kanssa, mikä tekee siitä vieläkin voimakkaamman työkalun merkkijonojen käsittelyssä.

## Katso myös

- [Rustin dokumentaatio](https://doc.rust-lang.org/std/primitive.str.html#method.substring)
- [Rustin merkkijonojen käsittely](https://www.freecodecamp.org/news/rust-string-vs-str/)

Kiitos lukemisesta! Toivottavasti tämä auttoi sinua ymmärtämään paremmin kuinka ottaa alamerkkijonoja merkkijonoista Rustissa. Hyödyntämällä `substring`-funktiota, voit käsitellä ja muokata merkkijonoja helposti ja tehokkaasti. Onnea ohjelmoinnissa!