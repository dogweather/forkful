---
title:                "Rust: Päätteen vastaavien merkkien poistaminen"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Oletko joskus törmännyt tilanteeseen, jossa haluat poistaa tietystä kaavasta tai kuvitteellisesta merkkijonosta vastaavia merkkejä? Kenties haluat siistiä merkkijonon tulostusta tai suorittaa tekstitiedostojen käsittelyä. Tässä blogikirjoituksessa tarkastelemme miten voit ratkaista tämän ongelman käyttämällä Rust-ohjelmointikieltä.

## Miten tehdä

Ensimmäiseksi, tarvitsemme jonkin verran taustatietoa String-tyypistä sekä patternilla-ohjelmalogiikasta. Pattern-metodi mahdollistaa merkkijonon tiettyjen merkkien korvaamisen toisilla. Tämä tarkoittaa usein merkkien poistoa, jos korvaavassa merkkijonossa ei ole mitään.

Aloitetaan luomalla String-tyyppinen muuttuja ja määrittämällä pattern, joka määrittelee poistettavien merkkien kaavan.

```Rust
let string = String::from("Tervetuloa blogiartikkeliin!");
let pattern = "aeiou";
```

Seuraavaksi voimme käyttää `replace`-metodia, joka korvaa merkkijonossa `string` löydetyt merkit `pattern`-muuttujalla. Tässä tapauksessa `pattern`-muuttuja määrittää poistettavat merkit, joten korvaavaksi merkkijonoksi jätämme tyhjän merkkijonon.

```Rust
let new_string = string.replace(pattern, "");
println!("{}", new_string);
```

Tuloste olisi seuraavanlainen:

```plaintext
Trvttl blgrttljn!
```

## Syvemmälle aiheeseen

Kun ymmärrät miten `replace`-metodi toimii, voit alkaa tutkimaan muita vaihtoehtoja. Esimerkiksi, jos haluat korvata merkkejä tilalle, voit antaa sille toisen merkkijonon `replace`-metodin toisena parametrina. Voit myös käyttää `regex`-kirjastoa, joka mahdollistaa monimutkaisempien kaavojen käytön.

Rustin standardikirjasto tarjoaa myös muita hyödyllisiä metodeja merkkijonojen käsittelyyn, kuten `trim` ja `slice`.

## Katso myös

- [String - Rustin dokumentaatio](https://doc.rust-lang.org/std/string/struct.String.html)
- [regex - Github](https://github.com/rust-lang/regex)
- [Rust Standardikirjasto](https://doc.rust-lang.org/std/)

Toivottavasti tämä blogikirjoitus auttoi sinua ymmärtämään miten voit poistaa merkkejä vastaavalla kaavalla Rustilla. Onnea ohjelmoinnissa!