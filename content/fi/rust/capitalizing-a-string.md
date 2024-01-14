---
title:                "Rust: Nauhoitteiden suurentaminen"
simple_title:         "Nauhoitteiden suurentaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Miksi Käyttää Rust-ohjelmointikieltä: Merkkijonon suurennuksen perusteet

Rust on nykyaikainen ja nopeasti kasvava ohjelmointikieli, joka tarjoaa tehokkaan vaihtoehdon C- ja C++-kielille. Se on tullut suosituksi erityisesti sen vahvan tyypityksen, turvallisuuden ja suorituskyvyn vuoksi. Yksi Rustin kätevistä ominaisuuksista on sen kyky käsitellä merkkijonoja helposti ja tehokkaasti. Tässä blogikirjoituksessa opit, miten pääset alkuun merkkijonojen suurennuksen kanssa Rustissa.

## Kuinka Suurentaa Merkkijono Rust-ohjelmoinnissa

Rustissa on sisäänrakennettu toiminto merkkijonon suurentamiseen, nimeltään `to_uppercase()`. Tämä toiminto ottaa merkkijonon vastaan parametrina ja palauttaa uuden merkkijonon, jossa kaikki kirjaimet on muutettu isoiksi. Seuraa alla olevia esimerkkejä ja niiden tulostusta ymmärtääksesi paremmin, kuinka `to_uppercase()` toimii.

```Rust
let lowercase_string = "Hei, maailma!";

// Käytetään to_uppercase() toimintoa suurentamaan merkkijono
let uppercase_string = lowercase_string.to_uppercase();

println!("Alkuperäinen merkkijono: {}", lowercase_string);
println!("Suurennettu merkkijono: {}", uppercase_string);
```

Tämä koodi tulostaa seuraavan:

```txt
Alkuperäinen merkkijono: Hei, maailma!
Suurennettu merkkijono: HEI, MAAILMA!
```

Voit myös käyttää `to_uppercase()` toimintoa merkkijonon osien suurentamiseen, jos haluat. Seuraavassa esimerkissä käytetään merkkijonolukua ja `to_uppercase()` toimintoa muuttamaan vain ensimmäinen kirjain isoksi.

```Rust
let lowercase_string = "tervetuloa";

// Käytetään to_uppercase() toimintoa suurentamaan vain ensimmäinen kirjain
let uppercase_string = lowercase_string[..1].to_uppercase() + &lowercase_string[1..];

println!("Alkuperäinen merkkijono: {}", lowercase_string);
println!("Suurennettu merkkijono: {}", uppercase_string);
```

Tulostus on seuraavanlainen:

```txt
Alkuperäinen merkkijono: tervetuloa
Suurennettu merkkijono: Tervetuloa
```

## Syvemmälle Merkkijonon Suurentamiseen

Merkkijonon suurentamisesta on hyötyä monissa erilaisissa tilanteissa, kuten käyttäjältä syötettyjen tietojen validoinnissa, tekstipohjaisissa sovelluksissa ja jopa yksinkertaisesti tietojen muokkaamisessa. On kuitenkin tärkeää huomata, että `to_uppercase()` toiminto ei muuta alkuperäistä merkkijonoa, vaan luo uuden suurennetun version. Jos haluat muuttaa alkuperäistä merkkijonoa suuriksi kirjaimiksi pysyvästi, voit käyttää `.to_uppercase()` metodin sijasta `make_ascii_uppercase()` metodia.

```Rust
let mut lowercase_string = String::from("kissa");

// Käytetään make_ascii_uppercase() metodia muuttamaan alkuperäinen merkkijono
lowercase_string.make_ascii_uppercase();

println!("Alkuperäinen merkkijono: {}", lowercase_string);
```

Tämä tulostaa:

```txt
Alkuperäinen merkkijono: KISSA
```

## Katso Myös

- [Rustin virallinen opas](https://doc