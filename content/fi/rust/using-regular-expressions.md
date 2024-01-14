---
title:                "Rust: Säännöllisten lausekkeiden käyttö"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Regular expressions ovat väline, jonka avulla voit tehokkaasti hakea ja manipuloida tekstiä. Niiden avulla voit etsiä tietynlaisia merkkijonoja ja suorittaa haluamiasi toimenpiteitä niiden kanssa. Rust-ohjelmointikieli tarjoaa myös loistavan työkalun regular expressionien käyttämiseen.

## Kuinka

Käytä regular expressioneja Rust-ohjelmointikielessä `regex` kirjaston avulla. Voit aloittaa tuomalla kirjaston käyttöön:
```Rust
use regex::Regex;
```
Sitten voit luoda regular expression käyttämällä `Regex::new` -metodia ja antamalla sille haluamasi merkkijonon, jota etsit:
```Rust
let re = Regex::new(r"Hello, world!").unwrap();
```
Tässä esimerkissä regular expression etsii tarkalleen merkkijonoa "Hello, world!".

Voit sitten käyttää `is_match` -metodia tarkistamaan, onko haluamasi merkkijono täsmälleen sama kuin regular expression:
```Rust
re.is_match("Hello, world!"); // palauttaa true
```

Voit myös käyttää `replace` -metodia korvaamaan haluamasi merkkijonon regular expressionilla:
```Rust
let replaced = re.replace_all("Hello, world!", "Hello, Rust!"); 
println!("{}", replaced); // tulostaa "Hello, Rust!"
```

## Syvemmälle

Regular expressioneilla on paljon erilaisia käyttötarkoituksia, ja niiden tehosta kannattaa lukea lisää. Voit esimerkiksi käyttää niitä tekstieditorissasi tekstien muokkaamiseen, tai automatisoimaan tietojenkäsittelyä jossakin projektissa. Tutustu `regex` kirjaston dokumentaatioon lisätietoja varten.

## Katso myös

- [`regex` kirjaston dokumentaatio]
- [Regular Expression Cheat Sheet]
- [Rust-ohjelmointikielen virallinen sivusto]

[`regex` kirjaston dokumentaatio]: https://docs.rs/regex/1.3.9/regex/
[Regular Expression Cheat Sheet]: https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/
[Rust-ohjelmointikielen virallinen sivusto]: https://www.rust-lang.org/