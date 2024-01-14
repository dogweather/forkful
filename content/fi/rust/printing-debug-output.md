---
title:    "Rust: Virheenkorjaustulosteen tulostaminen"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Rust on suosittu ohjelmointikieli sen nopeuden, muistinhallinnan ja monien muiden ominaisuuksien takia. Mutta miksi painaa debug-tulosteita, kun koodi jo toimii?

Vastaus on yksinkertainen: luotettavuus. Debug-tulostukset ovat erinomainen tapa varmistaa, että koodisi toimii odotetulla tavalla ja havaita mahdolliset virheet ennen kuin ne aiheuttavat suurempia ongelmia.

## Kuinka tehdä

Rustissa, painaminen debug-tulosteita tapahtuu käyttämällä `println!` makroa. Se noudattaa muotoa `println!("tulostettava teksti", muuttujat);` Joten jos haluat painaa muuttujan arvon debug-tulosteena, sinun tarvitsee vain lisätä sen jälkeen pilkun.

```Rust
fn main() {
  let x = 5;
  println!("Muuttujan x arvo on {}", x);
}
```

Tämä tulostaisi "Muuttujan x arvo on 5". Voit myös painaa useampia muuttujia lisäämällä lisää `{}` merkkejä ja lisäämällä vastaavat muuttujat pilkun jälkeen.

```Rust
fn main() {
  let x = 5;
  let y = 10;
  println!("Muuttujien x ja y arvot ovat {} ja {}", x, y);
}
```

Tämä tulostaisi "Muuttujien x ja y arvot ovat 5 ja 10". `println!` makro tukee myös muotoilumerkkejä, kuten `{:?}` joka tulostaa muuttujan `Debug`-ominaisuuden mukaisen arvon.

```Rust
fn main() {
  let x = 5;
  println!("Muuttujan x arvo on {:?}", x);
}
```

Tämä tulostaisi "Muuttujan x arvo on 5". Vaikka tämä saattaa vaikuttaa pieneltä erolta yksinkertaisesi `println!` käyttämiseen, se voi auttaa havaitsemaan virheitä ja ongelmia koodissasi.

## Syvällinen tutkimus

Rustissa, `println!` makron käyttö on osa `std` eli standardi kirjasto. Tämä on Rustin mukana tuleva kirjasto, joten sinun ei tarvitse asentaa tai tuoda sitä käyttöön. `println!` makron käyttö on myös yksinkertainen tapa lisätä loogisia tarkastuspisteitä koodiisi ja se on hyvä tapa tutustua uusiin muotoilumahdollisuuksiin.

Voit myös käyttää `eprint!` ja `eprintln!` makroja, jotka tulostavat lineaarisesti virheviestejä `stderr`-virtaan, joka on kätevä tapa saada nopea tarkistus siitä mikä osa koodistasi saattaa aiheuttaa ongelmia.

## Katso myös

- [Rust Reference: Debug and Formatting](https://doc.rust-lang.org/reference/macros/print.html)
- [The Rust Book: Basic Debugging](https://doc.rust-lang.org/book/ch05-00-structs.html#debugging-with-printing)
- [A Gentle Introduction to Debugging in Rust](https://www.youtube.com/watch?v=9_GUKDXdp-c)