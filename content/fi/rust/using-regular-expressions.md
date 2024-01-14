---
title:                "Rust: Säännöllisten lausekkeiden käyttäminen"
simple_title:         "Säännöllisten lausekkeiden käyttäminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita Rust-ohjelmoinnissa?

Säännölliset lausekkeet ovat tehokas tapa muotoilla ja hakea tietoa tekstistä. Ne ovat hyödyllisiä, kun halutaan tarkistaa tai manipuloida tekstiä esimerkiksi käyttäjän syötteiden tai tiedostojen käsittelyssä.

## Kuinka käyttää säännöllisiä lausekkeita Rustissa?

Käytännön esimerkissä luomme säännöllisen lausekkeen, joka tunnistaa sähköpostiosoitteet ja tulostaa ne konsoliin. Käytämme Rustin ```regex``` kirjastoa säännöllisen lausekkeen käsittelyyn.

```
// Tuodaan regex kirjasto Rustiin
use regex::Regex;

// Luodaan säännöllinen lauseke, joka hakee sähköpostiosoitteet
let re = Regex::new(r"[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}").unwrap();

// Testidataa, joka sisältää sähköpostiosoitteita
let data = "Sähköpostini on esimerkki@email.com, haluatko lähettää minulle viestin?";

// Käytetään find_iter -funktiota löytämään kaikki sähköpostiosoitteet
for email in re.find_iter(data) {
    println!("{}", email.as_str());
}

// Output:
// esimerkki@email.com
```

## Syvempi sukellus säännöllisiin lausekkeisiin Rustissa

Rustin ```regex``` kirjasto tarjoaa laajan valikoiman toimintoja säännöllisten lausekkeiden muodostamiseen ja käsittelyyn. Tarkempien ohjeiden ja esimerkkien löytämiseksi suosittelemme tutustumaan kirjaston [dokumentaatioon](https://docs.rs/regex/) ja [käyttöesimerkkeihin](https://rust-lang-nursery.github.io/rust-cookbook/text/regex.html).

## Katso myös

- [Rustin virallinen nettisivu](https://www.rust-lang.org/)
- [Rust-ohjelmoinnin aloitusopas](https://doc.rust-lang.org/book/)
- [Daily Rust -suomenkielinen yhteisö](https://dailyrust.com/)