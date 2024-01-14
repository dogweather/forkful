---
title:                "Rust: Merkkijonojen yhdistäminen"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

# Miksi

Miksi ihmiset haluaisivat yhdistellä merkkijonoja ohjelmoinnissa? Merkkijonojen yhdisteleminen on hyödyllinen ominaisuus, joka mahdollistaa tekstipohjaisten tietojen manipuloinnin ja muokkaamisen. Se on erityisen hyödyllinen tilanteissa, joissa tarvitaan dynaamisia tai vaihtuvia tietoja, kuten esimerkiksi käyttäjän syötteen lisääminen ohjelmaan.

# Miten tehdä

Rust-ohjelmointikieli tarjoaa useita tapoja yhdistää merkkijonoja. Yksi tapa on käyttää '+' operaattoria, joka yhdistää kaksi merkkijonoa yhteen. Toinen tapa on käyttää format!-makroa, joka sallii useiden muuttujien lisäämisen merkkijonoon. Alla on esimerkkejä kummastakin tavasta:

```Rust
let nimi = "Mikko";
let tervetuloa = "Tervetuloa, ";
let viesti = tervetuloa + nimi;

println!("{}", viesti);

// Tuloste: Tervetuloa, Mikko

let numero = 10;
let merkkijono = String::from("Tänään on ");
let pvm = format!("{} syyskuuta!", numero);

println!("{}{}", merkkijono, pvm);

// Tuloste: Tänään on 10 syyskuuta!
```
# Syvempi sukellus

Merkkijonojen yhdistäminen Rustissa voi joskus aiheuttaa ongelmia, jos esimerkiksi merkkijonot sisältävät ei-ASCII merkkejä. Tässä tilanteessa on suositeltavaa käyttää format_args!-makroa, joka palauttaa format_args-valikon. Tämä valikko voidaan sitten käyttää rakentamaan uutta merkkijonoa toiselle makrolle, kuten osämun.

Toinen hyödyllinen ominaisuus Rustissa on liitos-merkkijonoetäisyys. Tämä etäisyys määrittelee tavujen määrän, jonka jälkeen merkkijono muuttuu osämuksi, jos se täyttää tietyt ehdot. Tämä vähentää osämujen allokointia ja parantaa suorituskykyä.

# Katso myös

- [Rust-kieliohjeet merkkijonojen käsittelyyn](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- [Rustin merkkijonojen liittäminen, osämu ja liitos-etäisyyden](https://doc.rust-lang.org/rust-by-example/std/str.html#strings)