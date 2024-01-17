---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Rust: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Etsiminen ja korvaaminen tekstissä tarkoittaa tekstin tiettyjen merkkien tai sanojen etsimistä ja niiden korvaamista toisilla. Tämä on yleinen tehtävä ohjelmoinnissa, sillä se voi auttaa tehostamaan ja automatisoimaan tekstin muokkausta ja korjaamista.

## Miten:
Esimerkkejä koodista ja tulostuksesta ```Rust ...``` koodilohkoissa.

Etsiminen ja korvaaminen tekstissä voidaan toteuttaa Rustin standardikirjaston `replace()` funktiolla. Se ottaa kaksi merkkijonoa argumentteina ja palauttaa uuden merkkijonon, jossa kaikki ensimmäisen argumentin esiintymät on korvattu toisen argumentin sisältämällä merkkijonolla.

```
let original = "Tervetuloa, maailma!";
let uusi = original.replace("Tervetuloa", "Hei");
println!("{}", uusi); // tulostuu "Hei, maailma!"
```

## Syventyvä sukellus:
Etsiminen ja korvaaminen tekstissä on ollut osa ohjelmointia jo pitkään, ja sitä on toteutettu monilla eri tavoilla eri kielissä. Esimerkiksi Perlissä on omat funktiot tätä tarkoitusta varten. Lisäksi jotkut tekstieditorit, kuten Vim, tarjoavat myös mahdollisuuden etsiä ja korvata tekstiä.

## Katso myös:
- [Rustin dokumentaatio replace() funktiosta](https://doc.rust-lang.org/std/primitive.str.html#method.replace)
- [Perl-in-place -korvaus](https://www.perl.com/pub/2004/08/09/commandline.html/)
- [Vim - etsi ja korvaa](https://vim.fandom.com/wiki/Search_and_replace)