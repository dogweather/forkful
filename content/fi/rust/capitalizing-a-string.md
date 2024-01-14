---
title:                "Rust: Merkkijonon isokirjaimiseksi muuttaminen"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi käyttää Rustia merkkijonon muuttamiseksi ja yleistä tietoa Rustista
 
Rust on uusi ja innovatiivinen ohjelmointikieli, joka tarjoaa huomattavia parannuksia perinteisiin ohjelmointikieliin verrattuna. Yksi kätevimmistä tavoista käyttää Rustia on merkkijonon muuntaminen, joka voidaan suorittaa hyvin tehokkaasti ja turvallisesti. Tässä blogikirjoituksessa käymme läpi, miten voit käyttää Rustia merkkijonon muuttamiseen ja miksi se on niin hyödyllistä.

## Miten käyttää Rustia merkkijonon muuttamiseen

Rust tarjoaa useita sisäänrakennettuja työkaluja merkkijonon muuntamiseen, mukaan lukien `to_lowercase()`, `to_uppercase()` ja `capitalize()` -toiminnot. Alla on esimerkkikoodi, joka näyttää kuinka käyttää `capitalize()` -funktiota muuttamaan merkkijono ensimmäinen kirjain isolla kirjaimella.

```Rust
fn main() {
    let text = "hello world";
    let capitalized_text = text.capitalize();

    println!("{}", text);
    println!("{}", capitalized_text);
}
```

Tulostus on seuraava:

```
hello world
Hello world
```

Voit myös käyttää `to_lowercase()` ja `to_uppercase()` funktioita muuttaaksesi merkkijonon kaikki kirjaimet pieniksi tai isoiksi. Alla on esimerkki:

```Rust
fn main() {
    let text = "HELLO world";
    let lowercase_text = text.to_lowercase();
    let uppercase_text = text.to_uppercase();

    println!("{}", text);
    println!("{}", lowercase_text);
    println!("{}", uppercase_text);
}
```

Tulostus on seuraava:

```
HELLO world
hello world
HELLO WORLD
```

Nämä funktiot toimivat myös Unicode-merkkijonoilla, mikä tekee niistä hyvin monipuolisia työkaluja merkkijonon muuttamiseen.

## Syventävä sukellus merkkijonon muuttamiseen Rustissa

Rustilla on monia sisäänrakennettuja työkaluja merkkijonojen muuttamiseen, mutta voit myös luoda omia toimintoja käyttäen `chars()` -funktiota. Alla on esimerkki, joka muuttaa merkkijonon ensimmäisen kirjaimen isoksi kirjaimeksi omalla toiminnolla.

```Rust
fn capitalize_first_letter(text: &str) -> String {
    let mut characters = text.chars();

    match characters.next() {
        None => String::new(),
        Some(first_char) => first_char.to_uppercase()
                               .chain(characters)
                               .collect(),
    }
}

fn main() {
    let text = "hello world";
    let capitalized_text = capitalize_first_letter(text);

    println!("{}", text);
    println!("{}", capitalized_text);
}
```

Tulostus on seuraava:

```
hello world
Hello world
```

Tässä esimerkissä `to_uppercase()` toimii ensin ensimmäisellä kirjaimella ja `chain()` yhdistää sen loput kirjaimet takaisin alkuperäiseen merkkijonoon.

## Katso myös

- [Rust-ohjelmointikielen virallinen verkkosivusto](https://www.rust-lang.org/fi/)
- [Rust-ohjelmointikielen opetusohjelmat](https://doc.rust-lang.org/book/title-page.html)
- [Merkkijonojen käsittely Rustissa](https://www.rust-lang.org/fi/learn/tracks/string-essentials)