---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
date:                  2024-01-20T17:39:48.492908-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"

category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
"Mikä & Miksi?"

Muuttaa merkkijonon pieniksi kirjaimiksi. Tämä helpottaa vertailua ja hakua, koska isot ja pienet kirjaimet kohdellaan samoin.

## How to:
"Kuinka tehdä:"

```Rust
fn main() {
    let original = "Moikka Maailma!";
    let lowercased = original.to_lowercase();

    println!("{}", lowercased);
    // Tulostuu: "moikka maailma!"
}
```

## Deep Dive
"Sukellus syvyyksiin"

Rustin to_lowercase-funktio muuttaa merkkijonot pieniksi kirjaimiksi Unicode-standardin mukaisesti. Se ottaa huomioon eri kielet ja erikoistapaukset, ei vain ASCII-merkkejä. Tämä tehtiin siksi, että Rust tukee monia alustoja ja kulttuureja.

Vaihtoehtoja ovat mm. ASCII-toimintoja käyttävä to_ascii_lowercase, joka ei huomioi lokaalisia erityispiirteitä. Näin on olemassa kompromissi nopeuden ja täydellisen tuen välillä.

Mitä toteutukseen tulee, Rust käyttää tehokkaita iterointimekanismeja ja merkkijonojen käsittely on suunniteltu niin, että muistinkäyttö on optimaalinen ja turvallinen.

## See Also
"Katso myös"

- Rust-dokumentaatio to_lowercase-funktiosta: [https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase)
- Unicode-standardi: [http://www.unicode.org/](http://www.unicode.org/)
- Vertailu Rustin iterointimekanismeista: [https://doc.rust-lang.org/book/ch13-02-iterators.html](https://doc.rust-lang.org/book/ch13-02-iterators.html)
