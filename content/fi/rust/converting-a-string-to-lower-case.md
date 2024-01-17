---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "Rust: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Mikäli olet Rust ohjelmoija, sinun tulee varmasti vastaan tarve muuttaa merkkijono pieniksi kirjaimiksi. Tätä siistintä konversiota kutsutaan merkkijonon muuttamiseksi pieniksi kirjaimiksi. Tämä helpottaa mm. vertailua ja lajittelua kvantifioimattomien merkkien kesken.

## Miten:
Rustin standardi kirjasto tarjoaa `to_lowercase()` metodin, joka tekee halutun muunnoksen. Alla on yksinkertainen esimerkki käyttäen sitä:

```Rust
let s = String::from("HEI kaikki");
let s_lower = s.to_lowercase();

println!("Ennen: {}", s);
println!("Jälkeen: {}", s_lower);
```

Tämän koodinpätkän tulostus näyttäisi seuraavalta:

```
Ennen: HEI kaikki
Jälkeen: hei kaikki
```

## Syvä Sukellus:
Merkkijonon muuttaminen pieniksi kirjaimiksi on käytetty ohjelmointitekniikka, joka löytyy lähes joka kielestä. Alun perin se oli tarpeellinen tapa käsitellä merkkijonoja, joissa oli eri tapauksissa olevia kirjaimia (esim. "Hei" ja "hei"). Nykyään, se helpottaa myös merkkijonojen vertailua ja lajittelua.

Rustin `to_lowercase()` metodi käyttää Unicode-standardeihin perustuvaa algoritmia muuttingen. Tämä tarkoittaa, että se osaa käsitellä myös monimutkaisempia merkkejä, kuten aksentteja ja erikoismerkkejä.

## Katso myös:
- [Rustin string documentation](https://doc.rust-lang.org/std/string/struct.String.html)
- [MDN - String.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Unicode Standard Annex #29](https://unicode.org/reports/tr29/)