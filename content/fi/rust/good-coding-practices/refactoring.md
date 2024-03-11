---
date: 2024-01-26 03:36:56.872175-07:00
description: "Refaktorointi on olemassa olevan tietokonekoodin uudelleenrakentamisen\
  \ prosessi\u2014muuttamalla faktorointia\u2014muuttamatta sen ulkoista k\xE4ytt\xE4\
  ytymist\xE4.\u2026"
lastmod: '2024-03-11T00:14:30.299827-06:00'
model: gpt-4-0125-preview
summary: "Refaktorointi on olemassa olevan tietokonekoodin uudelleenrakentamisen prosessi\u2014\
  muuttamalla faktorointia\u2014muuttamatta sen ulkoista k\xE4ytt\xE4ytymist\xE4.\u2026"
title: Koodin refaktorointi
---

{{< edit_this_page >}}

## Mikä & Miksi?

Refaktorointi on olemassa olevan tietokonekoodin uudelleenrakentamisen prosessi—muuttamalla faktorointia—muuttamatta sen ulkoista käyttäytymistä. Ohjelmoijat tekevät tämän parantaakseen ohjelmiston ei-toiminnallisia ominaisuuksia, kuten luettavuutta, vähennettyä monimutkaisuutta, parannettua ylläpidettävyyttä ja luodakseen ilmaisuvoimaisemman sisäisen arkkitehtuurin tai objektimallin laajennettavuuden parantamiseksi.

## Kuinka:

Mennään refaktoroimaan yksinkertainen Rust-koodinpätkä, jotta se olisi idiomaattisempi ja ylläpidettävämpi. Aloiteamme funktiolla, joka laskee kokonaislukujen vektorin summan:

```rust
fn sum(vec: &Vec<i32>) -> i32 {
    let mut sum = 0;
    for i in vec {
        sum += i;
    }
    sum
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("Summa on {}", sum(&numbers));
}
```

Tuloste:
```
Summa on 15
```

Nyt refaktoroidaan tämä käyttämään idiomaattisempaa Rustia hyödyntämällä iteraattoreita ja `fold`-metodia:

```rust
fn sum(vec: &[i32]) -> i32 {
    vec.iter().fold(0, |acc, &x| acc + x)
}

fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    println!("Summa on {}", sum(&numbers));
}
```

Tuloksessa ei muutosta—se on edelleen `15`—mutta refaktoroitu versio on siistimpi ja käyttää Rustin vahvuuksia, kuten lainaamista ja iteraattorimetodeja.

## Syväsukellus

Refaktoroinnilla on juurensa Smalltalk-yhteisössä ja se popularisoitiin Java-maailmassa Martin Fowlerin kirjalla "Refactoring: Improving the Design of Existing Code". Sen periaatteet ovat universaaleja ja sovellettavissa myös Rustiin, missä turvallisuus ja rinnakkaisuus ovat ensiarvoisen tärkeitä. Rust kannustaa kirjoittamaan vankkaa koodia nappaamalla ongelmia käännösaikana, joten refaktoroinnin aikana Rust-kääntäjä toimii turvaverkkona.

Vaihtoehtoja manuaaliselle refaktoroinnille sisältävät automatisoidut työkalut, kuten 'rustfmt' koodin muotoilua varten ja 'clippy' linttausta varten, jotka voivat ehdottaa idiomaattisempia tapoja kirjoittaa koodia. Kuitenkin syvällinen refaktorointi vaatii usein harkittua ymmärrystä koodin suunnittelusta, minkä nämä työkalut eivät voi täysin automatisoida.

Rustissa refaktorointi saattaa keskittyä parantamaan tyyppien käyttöä, hyödyntämään eliniköjä tehokkaasti, vähentämään tarpeettomia allokointeja tai käyttämään rinnakkaisuusmalleja, kuten käyttämällä `Arc<Mutex<T>>` tarvittaessa. On myös yleistä siirtyä `unwrap()`-funktiosta ilmaisuvoimaisempaan virheenkäsittelyyn `Result<T, E>`-avulla.

## Katso myös

Syventääksesi ymmärrystäsi Rustin refaktoroinnista:

- Rust-kirja: https://doc.rust-lang.org/book/
- Rust esimerkein: https://doc.rust-lang.org/rust-by-example/
- Clippy, Rustin linttaustyökalu: https://github.com/rust-lang/rust-clippy
- "Refactoring: Improving the Design of Existing Code" Martin Fowlerilta: https://martinfowler.com/books/refactoring.html
