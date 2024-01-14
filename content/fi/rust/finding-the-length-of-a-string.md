---
title:    "Rust: Merkkijonon pituuden löytäminen"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi sinun pitäisi vaivautua selvittämään merkkijonon pituus Rust-ohjelmointikielellä? Koska merkkijonot ovat olennainen osa lähes jokaista ohjelmaa ja on tärkeää pystyä käsittelemään niitä tehokkaasti. Merkkijonon pituuden selvittäminen on yksi yleisimmistä tehtävistä, joita joudut tekemään ohjelmoinnin parissa, joten on hyödyllistä oppia kuinka se tehdään Rustilla.

## Kuinka tehdä

Merkkijonon pituuden selvittäminen Rustilla on yksinkertaista. Voit käyttää standardikirjaston `len()` -funktiota, joka palauttaa merkkijonon pituuden `usize`-tyyppisenä arvona. Esimerkiksi:

```Rust
let s = "Hei maailma!";

println!("Merkkijonon pituus: {}", s.len());
```

Tämä tulostaa seuraavan tekstin:

```
Merkkijonon pituus: 13
```

## Syvemmälle

Merkkijonon pituuden selvittäminen toimii siten, että jokaiselle merkille lasketaan yksi. Jokainen merkki käyttää muistissa yhden tavun verran. Tästä syystä merkkijonon pituus on yksi merkki enemmän kuin sen sisältämien merkkien määrä. Esimerkiksi merkkijono `"Hei"` sisältää neljä merkkiä, mutta sen pituus on viisi.

On myös hyvä huomata, että merkkijonon pituutta ei voi muuttaa, sillä se on osa Rustin `String`-tyyppiä, joka on muuttumaton.

## Katso myös

- [Rustin `String`-tyyppi ja sen toiminnot](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rustin standardikirjasto](https://doc.rust-lang.org/std/index.html)
- [Merkkijonojen muokkaaminen Rustilla](https://www.rust-lang.org/learn/strings)