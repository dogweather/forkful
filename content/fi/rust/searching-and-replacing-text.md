---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# # Mitä & Miksi?
Haku- ja korvaustoiminnot ihmisen ja tietokoneen välisessä kommunikoinnissa ovat tärkeitä tehtäviä, jotka etsivät tiettyjä merkkijonoja ja korvaavat ne toisilla halutuilla merkkijonoilla. Ohjelmoijat käyttävät näitä toimintoja usein koodin manuaalisen tekemisen välttämiseksi, jolloin työn tehokkuus kasvaa.

# # Kuinka:
Seuraavassa on esimerkki siitä, kuinka hakea ja korvata tekstiä Rust-koodikirjastossa.

```Rust
fn main() {
    let replace_in= "Tervetuloa Rust-ohjelmointiin!";
    println!("{}", replace_in.replace("Rust", "C++"));
}
```

Tämän koodiesimerkin tulosteena olisi:

```Rust
"Tervetuloa C++-ohjelmointiin!"
```

# # Syvempi tarkastelu

Haku- ja korvaustoiminnot ovat olleet tärkeä osa ohjelmointia jo pitkään. Sen avulla ohjelmoijat voivat korjata virheitä, muokata koodia uudelleenkäytettäväksi tai jopa kääntää sen toiselle kielelle. Rust-kielessä haku- ja korvaustoiminto tarjoaa joukon metodeja, joita voi käyttää merkkijonojen muokkauksessa.

Vaihtoehtoisesti, useissa muissa ohjelmointikielissä, kuten JavaScript ja Python, on samanlaisia hakukirjastoja merkkijonoille. Kuitenkin Rustin `replace()` -metodi on erityisen tehokas, koska se korvaa kaikki esiintymät, ei vain ensimmäistä.

On myös tärkeää huomata, että vaikka Rustin `replace()` -funktio on tehokas, se ei välttämättä sovellu kaikkiin tilanteisiin. Jos haluat esimerkiksi muokata merkkijonon osia monimutkaisilla säännöllisillä lausekkeilla, saattaa olla järkevämpää käyttää Rustin regex-kirjastoa.

# # Katso myös:

- [Rustin dokumentaatio merkkijonojen muokkaamisesta](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [JavaScript String Replace -metodi](https://www.w3schools.com/jsref/jsref_replace.asp)
- [Python string replace -metodi](https://docs.python.org/3/library/stdtypes.html#str.replace)