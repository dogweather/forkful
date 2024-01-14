---
title:    "Rust: Tekstin hakeminen ja korvaaminen"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi käyttää tekstinhakua ja korvausta

Tekstinhaku ja korvaus ovat tärkeitä työkaluja, jotka auttavat helpottamaan ohjelmointikokemusta. Ne voivat auttaa nopeuttamaan koodin kehitystä ja vähentämään inhimillisiä virheitä.

## Miten tehdä tekstinhakua ja korvausta Rustilla

Rustin sisäänrakennettu `replace`-metodi on yksi tapa suorittaa tekstinhakua ja korvausta. Sen avulla voit vaihtaa haluamasi näytön kaikki esiintymät toiseen merkkijonoon.

```
let teksti = "Tervetuloa Rust-ohjelmointiin!";
let uusi_teksti = tekstin.replace("Rust", "ohjelmointi");

println!("{}", uusi_teksti);
// Tulostaa: Tervetuloa ohjelmointi-ohjelmointiin!
```

Voit myös käyttää `find`- ja `replace_range`-metodeja, jos haluat tarkemman kontrollin tulokseen.

```
let teksti = "Tässä on teksti, jossa haetaan ja korvataan merkkijonoja.";
let indeksi = teksti.find("ja").unwrap(); // Etsitään indeksi, jossa "ja" esiintyy

let (ensimmäinen_osuus, toinen_osuus) = teksti.split_at(indeksi); // Jaetaan teksti kahteen osuuteen indeksin kohdalta
let uusi_teksti = ensimmäinen_osuus.to_string() + "tai" + &toinen_osuus[2..]; // Vaihdetaan "ja" sanaksi "tai"

println!("{}", uusi_teksti);
// Tulostaa: Tässä on teksti, jossa haetaan tai korvataan merkkijonoja.
```

## Syvä sukellus tekstinhakuun ja korvaukseen

Tekstinhaku ja korvaus voivat olla hyödyllisiä esimerkiksi tekstipohjaisissa peleissä, joissa pelaajan antama teksti tulee tarkistaa ja korvata esimerkiksi tenämerkeillä tai piiloreiteillä. Ne voivat myös auttaa ohjelmoijaa muokkaamaan tekstitiedostoja, jotta ne vastaavat paremmin tietyille kriteereille.

## Katso myös

- [Rustin virallinen opas tekstinhakuun ja korvaukseen](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Rustin String-tyyppi](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rustin Vec-tyyppi](https://doc.rust-lang.org/std/vec/struct.Vec.html)