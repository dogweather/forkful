---
title:    "Rust: Säännöllisten ilmaisujen käyttö"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita (regular expressions)?

Säännölliset lausekkeet ovat voimakas työkalu, joka mahdollistaa tietyn kaavan tai rakenteen löytämisen tekstimassasta. Niitä käytetään usein tiedonkäsittelyssä, kuten tiedostonmuokkauksessa tai tietokantojen kyselyissä. Säännöllisten lausekkeiden avulla pystyt käsittelemään suuria määriä dataa nopeasti ja tehokkaasti.

## Näin käytät säännöllisiä lausekkeita Rustissa

Säännöllisten lausekkeiden käyttö Rustissa on helppoa. Sinun tulee ensin tuoda `regex`-kirjasto projektiisi. Tämän jälkeen voit käyttää `Regex`-luokkaa luomaan säännöllisiä lausekkeita ja suorittamaan hakuja tekstissä.

```Rust
// Tuodaan regex-kirjasto käyttöön
use regex::Regex;

// Luodaan uusi säännöllinen lauseke, joka etsii kaikki numerot tekstistä
let re = Regex::new(r"\d+").unwrap();

// Suoritetaan haku annetusta tekstistä
let result = re.find("123 lorem ipsum").unwrap();

// Tulostetaan löydetty tulos
println!("Löydetty teksti: {}", result.as_str());
```

Tässä esimerkissä säännöllinen lauseke etsii kaikki numerot annetusta tekstistä ja tulostaa löydetyn numeron.

## Syvempi sukellus säännöllisten lausekkeiden käyttöön

Säännölliset lausekkeet ovat tehokas työkalu tekstin käsittelyssä, mutta niiden käyttöön kannattaa tutustua huolellisesti. On tärkeää ymmärtää säännöllisten lausekkeiden syntaksi ja miten niitä voi hyödyntää erilaisissa tilanteissa.

Rustissa `regex`-kirjasto tarjoaa laajan valikoiman erilaisia metodeja ja ominaisuuksia säännöllisten lausekkeiden käyttöön. Näihin kuuluu esimerkiksi `replace`-metodi, jolla voit korvata löydetyt tekstipätkät haluamallasi merkkijonolla. Voit myös antaa säännölliselle lausekkeelle erilaisia vaihtoehtoisia muotoiluja, jotka lisäävät sen tehokkuutta ja tarkkuutta.

## Katso myös

[Regex-dokumentaatio Rustille](https://docs.rs/regex/)

[Rust-ohjelmointikielen virallinen verkkosivusto](https://www.rust-lang.org/fi/)

[Säännöllisten lausekkeiden perusteet](https://www.regular-expressions.info/fi/index.html)