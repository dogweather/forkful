---
title:                "Pohjaparametrien mukaisen tekstin poistaminen"
html_title:           "Rust: Pohjaparametrien mukaisen tekstin poistaminen"
simple_title:         "Pohjaparametrien mukaisen tekstin poistaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä miksi voit haluta poistaa merkkejä jotka vastaavat tiettyä kaavaa. Saattaa olla että haluat puhdistaa tietoa ennen sen käyttöä, tai ehkä haluat tehdä tietokannan kyselyn tehokkaammin.

## Miten

Rustissa merkkijonojen käsittely on tehty helpoksi. Voit käyttää `replace` metodia ja antaa sille kaksi argumenttia: kaavan jonka haluat poistaa ja sen tilalle laitettavan tyhjän merkkijonon. Katso esimerkki alla:

```Rust
let text = "Tämä on esimerkkilause.";
let uusi_text = text.replace("esimerkki", "uusi");
println!("{}", uusi_text); 
// Tulostaa: Tämä on uusi lause.
```

Simple ja kätevää, eikö? Voit myös käyttää regexiä poistaaksesi monimutkaisempia kaavoja. Tässä esimerkki:

```Rust
use regex::Regex;

let text = "Hei, minun nimeni on Pauli. Miten voin auttaa?";
let regex = Regex::new(r"[aeiou]").unwrap();
let uusi_text = regex.replace_all(text, "");
println!("{}", uusi_text);
// Tulostaa: H, mn nmnn Pll. Mtn vn ttn?
```

## Syvällisemmin

`replace` metodi palauttaa uuden merkkijonon sen sijaan että muuttaa sitä alkuperäisen muuttujan arvoa. Voit myös käyttää `replace_first` metodia joka korvaa vain ensimmäisen löydetyn kaavan. Lisäksi, Rustin standardikirjasto tarjoaa myös `drain_filter` metodin joka poistaa alkuperäisen merkkijonon arvon.

## Katso myös

- [Rust ohjelmointikielen virallinen dokumentaatio](https://www.rust-lang.org/fi)
- [Regex kirjaston dokumentaatio](https://docs.rs/regex/1.3.1/regex/)