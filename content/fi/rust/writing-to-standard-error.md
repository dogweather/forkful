---
title:    "Rust: Kirjoittaminen standardivirheeseen"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen standardivirheeseen (standard error) on tärkeä osa ohjelmointia Rust-kielellä. Se on tapa ilmoittaa virheistä ja muista tärkeistä tiedoista käyttäjälle tai ohjelman kehittäjälle. Tämä tieto näytetään komentorivillä, eikä se sekoitu ohjelman tuottamaan tavalliseen ulostuloon.

## Näin teet sen

Seuraa näitä ohjeita kirjoittaaksesi standardivirheeseen käyttäen Rustia:

```
Rust fn main() {
    eprintln!("Tämä on virheilmoitus standardivirheeseen.");
}
```

Kun suoritat tämän koodin, näet seuraavan tulosteen standardivirheessä:

```
Tämä on virheilmoitus standardivirheeseen.
```

Voit myös lisätä muita tietoja tai muuttujia virheilmoitukseen:

```
Rust fn main() {
    let virheilmoitus = "Tämä on virheilmoitus standardivirheeseen.";
    eprintln!("{} - Tärkeä huomautus!", virheilmoitus);
}
```

Tulostus:

```
Tämä on virheilmoitus standardivirheeseen. - Tärkeä huomautus!
```

## Syvemmälle

Kirjoittaminen standardivirheeseen on hyödyllinen tapa ohjelman kehittäjälle tai käyttäjälle saada tietoa ohjelman suorituksen aikana tapahtuvista virheistä ja muista tärkeistä tiedoista. On tärkeää käyttää `eprintln!` -makron sijaan `println!` -makroa, jotta nämä tiedot eivät sekoitu tavallisen ulostulon kanssa. Voit myös käyttää `panic!` -makroa tulostamaan virheilmoituksen ja lopettamaan ohjelman suorituksen, jos tarpeellista.

## Katso myös

- [Rust-in-kissat: Kirjoittaminen standardivirheeseen](https://rust-in-kissat.com/standard-virheet)
- [Rust-ohjelmointikielen virallinen sivusto](https://www.rust-lang.org)
- [Rust-viljelmät: Virheiden käsittely](https://rust-viljelmat.org/virheiden-käsittely.html)