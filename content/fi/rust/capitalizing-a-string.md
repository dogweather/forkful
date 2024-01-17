---
title:                "Stringin ensimmäisten kirjainten merkitys"
html_title:           "Rust: Stringin ensimmäisten kirjainten merkitys"
simple_title:         "Stringin ensimmäisten kirjainten merkitys"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Mitä & Miksi?
Tekstijonojen muuttaminen isolla alkukirjaimella viittaa siihen, että kaikki jonojen ensimmäiset kirjaimet muutetaan isoksi. Ohjelmoijat tekevät tätä yleensä yhdenmukaistamaan ja helpottamaan jonojen vertailua ja käsittelyä.

Kuinka:
Esimerkki 1:

```Rust
let string = "tämä on tekstijono";
let capitalized_string = string.to_uppercase();
println!("Alkuperäinen jono: {}", string);
println!("Isolla alkukirjaimella: {}", capitalized_string);
```
Output:
```
Alkuperäinen jono: tämä on tekstijono
Isolla alkukirjaimella: TÄMÄ ON TEKSTIJONO
```

Esimerkki 2:
```Rust
fn capitalize(string: &str) -> String {
    let mut new_string = String::from(string);
    new_string.replace_range(0..1, &string[0..1].to_uppercase());
    new_string
}

println!("{}", capitalize("tämä on toinen esimerkki"));
```

Output:
```
"Tämä on toinen esimerkki"
```

Deep Dive:
Tekstijonojen muuttaminen isolla alkukirjaimella on osa laajempaa tietojen käsittelyä ohjelmoinnissa. Se auttaa yhdenmukaistamaan erilaisia tietoja ja helpottamaan niiden vertailua ja käsittelyä. Tekniikka on ollut käytössä jo pitkään ja sitä käytetään eri ohjelmointikielissä.

Vaihtoehtoisia tapoja muuttaa tekstijonoja isolla alkukirjaimella on esimerkiksi käyttää merkkijonoitten manipulointiin tarkoitettuja kirjastoja tai itse kirjoittaa algoritmi, joka tekee muutokset. Rustissa on valmiita toimintoja, kuten `to_uppercase()`, jotka helpottavat tätä.

Tekstiä käsiteltäessä Rustissa on hyvä tietää, että merkkijonojen sisältämä data on pitkälti muuttumaton. Tämä tarkoittaa sitä, että kun muokkausoperaatio tehdään merkkijonolle, siitä ei luoda uutta kopioita vaan tehdään muutokset alkuperäisessä muuttumattomassa merkkijonossa.

See Also:
- [Rust String Documentation](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rust String Methods](https://doc.rust-lang.org/std/string/struct.String.html#impl-String)
- [Rust String Manipulation Libraries](https://crates.io/keywords/string-manipulation)