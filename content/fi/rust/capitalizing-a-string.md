---
title:    "Rust: Tekstin muuttaminen isoiksi kirjaimiksi"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluat mahdollisesti muuttaa merkkijonon ensimmäisen kirjaimen isoksi. Esimerkiksi jos rakennat sovellusta, jossa käyttäjät kirjoittavat tietoja, haluat ehkä varmistaa että kaikki syötetyt tiedot ovat yhtenäisessä muodossa.

## Kuinka se tehdään

Rust-ohjelmointikielen avulla voit muuttaa merkkijonon ensimmäisen kirjaimen isoksi helposti käyttämällä String-tyyppisen muuttujan capitalize() -metodia. Esimerkiksi:

```Rust
let s = String::from("moikka");

println!("{}", s.capitalize()); // Tulostaa "Moikka"
```

Jos haluat muuttaa vain yhden tietyn kirjaimen isoksi, voit käyttää replace_range() -metodia. Esimerkiksi:

```Rust
let mut s = String::from("moikka");
s.replace_range(0..1, "M");

println!("{}", s); // Tulostaa "Moikka"
```

Voit myös hyödyntää Rustin str-metodeja, kuten char_at() ja to_uppercase(), muuttaaksesi yksittäisen kirjaimen isoksi. Esimerkiksi:

```Rust
let mut s = String::from("moikka");
let first_char = s.char_at(0);
s.replace_range(0..1, first_char.to_uppercase());

println!("{}", s); // Tulostaa "Moikka"
```

## Syvemmälle aiheeseen

Merkkijonon ensimmäisen kirjaimen isoksi muuttamisen taustalla on Unicode-standardi. Rustissa merkkijonot ovat tavallaan kokoelma Unicode-merkkejä, jotka muodostavat kirjaimia, numeroita ja muita symboleja. Rustin capitalize() -metodi käyttää Unicode-standardia määrittääkseen, mikä merkki on tarkalleen ottaen merkkijonon ensimmäinen kirjain.

Rustin to_uppercase() -metodi taas käyttää erillistä ASCII-standardia, jota se käyttää muuttaessaan yksittäisen kirjaimen isoksi. Tämä voi olla hyödyllistä etenkin, kun käsittelet vain ASCII-merkkejä sisältäviä merkkijonoja.

## Katso myös

- [Unicode-standardi](https://unicode.org/)
- [ASCII-standardi](https://www.ascii-code.com/)