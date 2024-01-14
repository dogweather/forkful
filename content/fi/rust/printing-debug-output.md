---
title:                "Rust: Tulostetaan debug-tulosteita"
simple_title:         "Tulostetaan debug-tulosteita"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

Finnish: ## Miksi

Miksi tulostamaan debuggaustulostusta? Debuggaus on tärkeä osa ohjelmoinnin prosessia, ja tulostamalla debuggaustulostusta voidaan selvittää ohjelman toimintaa ja mahdollisia virheitä.

## Kuinka

Ohjelmoidessa Rustilla, voit tulostaa debuggaustulostusta käyttämällä `println!` makroa. Alla on esimerkkejä koodista ja tulostuksista:
```Rust
let nimi = "Matti";
let ikä = 24;

println!("Terveisiä, olen {} ja olen {} vuotta vanha.", nimi, ikä);
```
Tulostus:
```
Terveisiä, olen Matti ja olen 24 vuotta vanha.
```
Voit myös tulostaa muuttujan tai rakenteen arvon käyttämällä `{:?}` merkintää:
```Rust
let lista = [1, 2, 3];
let vektori = vec![4, 5, 6];

println!("Lista: {:?}, Vektori: {:?}", lista, vektori);
```
Tulostus:
```
Lista: [1, 2, 3], Vektori: [4, 5, 6]
```

## Syvällisemmältä

Tulostamalla debuggausarvoja, voit selvittää tarkemmin ohjelmasi toimintaa ja mahdollisia virheitä. Voit myös käyttää erilaisia makroja kuten `dbg!` tai `eprintln!`, jotka antavat lisätietoja tulosteeseen.

Lisäksi voit käyttää `#[derive(Debug)]` annotaatiota rakenteiden ja enumien yhteydessä, jolloin voit tulostaa koko rakenteen tai enumin kerralla.

## Katso myös

- [Rust dokumentaatio](https://www.rust-lang.org/learn)
- [Rust ohjelmoinnin käytännön esimerkkejä](https://github.com/rust-lang/rustlings)
- [Rust yhteisö ja keskustelufoorumi](https://users.rust-lang.org/)
- [Rust ohjelmointikielen opiskelijalle](https://www.tutorialspoint.com/rust/index.htm)