---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Rust: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Regular expressioneiden käyttö on yksi tehokkaimmista tavoista käsitellä tekstiä ja datan kanssa ohjelmoinnissa. Ne mahdollistavat monimutkaisten haku- ja muokkaustoimintojen suorittamisen yhdellä rivillä koodia.

## Kuinka käyttää

```Rust
use regex::Regex;

fn main() {
    // Luodaan regexp-olio, joka etsii kaikki "rust"- esiintymät tekstissä
    let re = Regex::new(r"rust").unwrap();

    // Määritetään teksti, josta halutaan etsiä esiintymiä
    let text = "Tervetuloa Rustin maailmaan! Oletko jo kokeillut Rustia?";

    // Suoritetaan haku tekstistä ja tulostetaan löydetyt esiintymät
    for mat in re.find_iter(text) {
        println!("Löydettiin esiintymä: {}", mat.as_str());
    }
}
```
```
Output:
Löydettiin esiintymä: Rust
Löydettiin esiintymä: Rust
```

Voit myös käyttää regular expressioneita muokkaamaan tekstiä haluamallasi tavalla. 

```Rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"(\w+) (\w+)").unwrap();

    let text = "Tervetuloa Rustin maailmaan!";

    // Korvataan ensimmäinen sana toisella sanalla.
    let new_text = re.replace(text, "Hei $2,$1!");

    println!("{}", new_text);
}
```
```
Output:
Hei maailmaan,Rustin!
```

## Syvempi sukellus

Regular expressioneita käyttäessä on tärkeää ymmärtää niiden syntax, jotta voit luoda tehokkaita haku- ja muokkaustoimintoja. Rustissa regular expressioneja käsitellään regex-kirjaston avulla, joka tarjoaa monia hyödyllisiä metodeja ja toimintoja.

Kannattaa myös tutustua erilaisiin regex-koukeroihin, kuten säännöllisiin lausekkeisiin ja cattymallisiin. Näillä voi olla suuri merkitys, kun pyrit luomaan tarkkoja hakuja ja sääntöjä.

## Katso myös

- [Rustin virallinen verkkosivusto](https://www.rust-lang.org/fi)
- [Regex-kirjaston dokumentaatio](https://docs.rs/regex/1.4.3/regex/)

Voit myös löytää monia hyödyllisiä esimerkkejä regular expressioneiden käytöstä Githubista tai Stack Overflow -sivustolta. Onnea matkaan regular expressioneja hyödyntävässä ohjelmoinnissa!