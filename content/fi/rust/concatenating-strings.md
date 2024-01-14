---
title:    "Rust: Stringien yhdistäminen"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

# Miksi käyttää Rustia merkkijonojen yhdistämiseen?

Merkkijonot ovat oleellinen osa ohjelmointia, ja niitä tarvitaan usein yhdistämään tekstejä tai muuttujien arvoja. Rust on suosittu ohjelmointikieli, joka tarjoaa tehokkaan ja turvallisen tavan käsitellä merkkijonoja, mukaan lukien niiden yhdistäminen. Joten jos haluat kirjoittaa turvallista ja vakaata koodia, kannattaa harkita Rustin käyttöä merkkijonojen yhdistämiseen.

# Miten yhdistää merkkijonoja Rustilla?

Rustissa merkkijonojen yhdistäminen tapahtuu ```+``` -operaattorin avulla. Tämä operaattori yhdistää kaksi merkkijonoa ja palauttaa uuden yhdistetyn merkkijonon. Katsotaan esimerkkiä:

```rust
let s1 = "Tervetuloa";
let s2 = "Rust-ohjelmointimaailmaan!";
let result = s1 + " " + s2;
```

Tässä esimerkissä luodaan kaksi merkkijonoa ```s1``` ja ```s2``` ja yhdistetään ne ```result``` -muuttujaan. Lopputuloksena tulisi olla uusi merkkijono ```Tervetuloa Rust-ohjelmointimaailmaan!```

# Syvällisempi tarkastelu Rustin merkkijonojen yhdistämisestä

Rustissa merkkijonot ovat osa String-tietotyyppiä, joka on muokattavissa oleva ja omistettu merkkijono. Tämä tarkoittaa, että kun yhdistät merkkijonoja, alun perin määriteltyä merkkijonoa ei muuteta vaan luodaan uusi merkkijono. Tämä on erityisen tärkeää turvallisuuden kannalta, sillä se estää muistiongelmien syntymistä.

Lisäksi, Rustissa on myös ```format!``` -makro, joka tarjoaa toisen tavan yhdistää merkkijonoja. Tämä makro ottaa vastaan muuttujia ja tekstipätkiä ja palauttaa yhdistetyn merkkijonon. Esimerkiksi:

```rust
let nimi = "Matti";
let ikä = 30;
let info = format!("Nimeni on {} ja olen {} vuotta vanha.", nimi, ikä);
```

Tämä tuottaisi tuloksen ```Nimeni on Matti ja olen 30 vuotta vanha.```

# Katso myös

- [Rust-ohjelmointikielen virallinen verkkosivusto] (https://www.rust-lang.org/)
- [Rustin dokumentaatio merkkijonoista] (https://doc.rust-lang.org/stable/std/string/index.html)
- [Rust-ohjelmointimaailman suomenkielinen yhteisö] (https://rust-suomi.github.io/)

Kiitos lukemisesta ja onnea Rustin oppimisen kanssa!