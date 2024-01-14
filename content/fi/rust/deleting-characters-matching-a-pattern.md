---
title:    "Rust: Kuvioon vastaavien merkkien poistaminen"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Miksi
Monet kehittäjät käyttävät Rustia sen nopeuden ja turvallisuuden takia. Tässä artikkelissa syvennymme yhteen näistä ominaisuuksista ja puhumme siitä, miksi saattaisit haluta poistaa merkkejä, jotka vastaavat tietyssä kaavassa.

## Kuinka
Kun työskentelet Rustin kanssa, saatat törmätä tilanteeseen, jossa haluat poistaa tietyn kaavan mukaiset merkit merkkijonosta. Tämä on helppo toteuttaa Rustin `replace`-funktion avulla. Katsotaanpa esimerkkikoodia:

```Rust
let merkkijono = "Tämä on esimerkkilausetta! #funktio #ohjelmointi";

// Haluamme poistaa kaikki #-merkit ja sitä seuraavat sanat
let muokattu_merkkijono = merkkijono.replace(Regex::new(r"#\w+").unwrap(), "");

println!("{}", muokattu_merkkijono);

// Output: "Tämä on esimerkkilausetta! "
```

Kuten näemme, ensin määritellään merkkijono, josta haluamme poistaa merkit. Sitten käytämme `replace`-funktiota, jossa annamme sen tietää, mitä haluamme korvata (`Regex::new`) ja millä haluamme korvata (`""`). Lopuksi tulostamme uuden muokatun merkkijonon konsoliin.

## Syvempi sukellus
Tämä saattaa vaikuttaa yksinkertaiselta, mutta on tärkeää ymmärtää, että `replace`-funktio palauttaa uuden muokatun merkkijonon eikä muuta alkuperäistä merkkijonoa. Se myös käsittelee kaavaa `&str`-muodossa, joten sen pitää olla `Regex`-tyyppiä. Jos et ole varma haluamastasi kaavasta, voit käyttää `replace_all`-funktiota, joka korvaa kaikki vastaavat kaavat merkkijonossa.

## Katso myös
- [Regex Crate dokumentaatio](https://crates.io/crates/regex)
- [Rustin opas - merkkijonot](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Merkkijonojen käsittely Rustissa](https://www.ralfebert.de/snippets/rust/regex_string_matches/)