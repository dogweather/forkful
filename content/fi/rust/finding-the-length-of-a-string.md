---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonojen pituuden selvittämisessä on kyse merkkien lukumäärän laskemisesta jokaiselle merkkijonolle. Tämä on oleellista esimerkiksi silloin, kun halut järjestää tai verrata merkkijonoja, tai varmistaa, että ne mahtuvat tiettyyn tilaan.

## Miten:

Rustissa voit hakea merkkijonon pituuden `.len()` -metodilla. Tässä on esimerkki sen käytöstä:

```Rust
fn main() {
    let s = "Tervetuloa";
    println!("{}", s.len());
}
```

Tämä tuottaa tulosteen `11`, koska `"Tervetuloa"` sisältää 11 merkkiä.

## Syvempää tietoa:

Historiallinen viite: Rust-ohjelmointikielessä merkkijonot ovat UTF-8-koodattuja, joten merkkijonon pituus voidaan laskea nopeasti ottamatta huomioon monitavuisia merkkejä.

Vaihtoehtoja: Rustissa on olemassa myös `.chars().count()` -tekniikka, joka laskee todelliset Unicode-skalaarit, mutta se on suhteellisen hidas
```Rust
fn main() {
    let s = "こんにちは";
    println!("{}", s.chars().count());
}
```
Tämän esimerkin tulostus olisi `5`, koska jokainen merkki koostuu useista biteistä.

Merkkijonojen pituuden määrittäminen perustuu sisäisesti kapasiteetin ja aloituspisteen eron laskemiseen.

## Katso myös:

[Rust-ohjelmointidokumentaatio, String](https://doc.rust-lang.org/std/string/)

[Rust by Example, Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)

[Rust-turvatyypit, String](https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html)