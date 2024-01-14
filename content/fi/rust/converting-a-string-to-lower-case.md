---
title:    "Rust: Muuntamalla merkkijono pienaakkosiksi"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Miksi: Merkitys tekstin muuntamisessa pienempiin kirjaimiin

Rust on suosittu, nopea ja turvallinen ohjelmointikieli, joka tarjoaa monipuolisia ominaisuuksia kehittäjille. Yksi näistä ominaisuuksista on kyky muuttaa tekstiä pienemmiksi kirjaimiksi, eli muuntaa se pieniksi kirjaimiksi. Tässä blogikirjoituksessa kerromme, miksi ja miten voit muuntaa merkkijonon pienemmiksi kirjaimiksi käyttämällä Rustia.

## Miten tehdä: Käytännön esimerkkejä ja tulosteet "```Rust ... ```"-koodilohkoissa

Merkkijonon muuntaminen pienemmiksi kirjaimiksi on hyödyllinen taito, jota tarvitaan usein ohjelmoinnissa. Se voi auttaa esimerkiksi vertaamaan merkkijonoja ilman, että isot ja pienet kirjaimet vaikuttavat tulokseen. Voit käyttää Rustia muuttaaksesi merkkijonoja pienempiin kirjaimiin helposti ja tehokkaasti. Katso esimerkkikoodi ja tulosteet alla, miten tämä voidaan tehdä Rustilla:

```
Rust
let teksti = "TÄMÄ ON MERKKIJONO";
let pienet_kirjaimet = teksti.to_lowercase();
println!("{}", pienet_kirjaimet);
```

Tuloste:
```
tämä on merkkijono
```

## Syvällinen sukellus: Lisätietoja merkkijonon muuntamisesta pienemmiksi kirjaimiksi

Rustilla on sisäänrakennettu toiminto `to_lowercase()`, joka muuttaa merkkijonon pienemmiksi kirjaimiksi. Tämä toiminto käyttää Unicode-standardia muuntaessaan merkkijonon, joten se toimii hyvin eri kielillä ja merkistöillä. Lisäksi tämä toiminto ei muuta alkuperäistä merkkijonoa, vaan palauttaa uuden muunnetun merkkijonon.

On myös huomattava, että tätä toimintoa voidaan käyttää vain `strings::String`-tietotyypin kanssa, ei esimerkiksi `&str`-tietotyypin kanssa. Jos haluat käyttää sitä `&str`-tietotyypillä, voit ensin muuttaa merkkijonon `String`-muotoon ja sitten muuntaa sen pienemmiksi kirjaimiksi.

Tietenkin on myös mahdollista muuttaa merkkijonon kirjainkoko manuaalisesti iteraation avulla ja käyttäen esimerkiksi `to_ascii_lowercase()`-toimintoa, mutta `to_lowercase()` on helpoin ja suositeltavin vaihtoehto.

## Katso myös

- Rustin virallinen dokumentaatio `to_lowercase()`-funktiosta: https://doc.rust-lang.org/stable/std/string/trait.ToLowerCase.html
- Artikkeli merkkijonon muuntamisesta pienemmiksi kirjaimiksi Rustilla: https://techblog.theslowloris.com/convert-a-string-to-lowercase-in-rust/
- Kielenvaihto merkkijonon muuntamisessa Rustilla: https://blog.alexandergottlieb.com/rust-change-string-to-lower-unicode-case-c1b66eb96958