---
title:    "Rust: Merkitsevän merkkijonon muokkaaminen"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Miksi Rustilla kannattaa muuttaa merkkijonon ensimmäinen kirjain isoksi?

## Miten tehdä
Esimerkki koodilla ja tulosteella, jotka näyttävät, miten merkkijonon ensimmäinen kirjain muutetaan isoksi Rustilla:

```Rust
fn main() {
    let s = "rusti";
    let s_capitalized = s.to_uppercase();
    
    println!("Alkuperäinen merkkijono: {}", s);
    println!("Isoksi muutettu merkkijono: {}", s_capitalized);
}
```
Tämä koodi tulostaa:

```
Alkuperäinen merkkijono: rusti
Isoksi muutettu merkkijono: RUSTI
```

## Syvällisempi sukellus
Muuttaessamme merkkijonon ensimmäinen kirjain isoksi Rustilla, käytämme `to_uppercase()` -metodia, joka palauttaa uuden merkkijonon. Tämä metodi toimii Unicode merkeillä ja se osaa myös käsitellä erikoismerkkejä ja akkusatiiveja. 

Tässä esimerkissä haluamme muuttaa merkkijonon ensimmäisen kirjaimen isoksi, mutta Rustilla on myös muita tapoja muokata merkkijonoja. Voimme esimerkiksi käyttää `to_lowercase()` -metodia, joka muuttaa kaikki merkit pieniksi, tai `replace()` -metodia, joka korvaa tietyn merkkijonon toisella.

## Katso myös
- Rustin virallinen dokumentaatio merkkijonojen käsittelystä: https://doc.rust-lang.org/std/string/
- Rustin vakiotoimintojen kirjasto: https://doc.rust-lang.org/std/
- YouTube-video "Rust programming - Strings" (englanniksi): https://www.youtube.com/watch?v=wjAM8JKEDJM