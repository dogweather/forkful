---
title:    "Rust: Säännöllisten lausekkeiden käyttö"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita Rust-ohjelmoinnissa?

Säännölliset lausekkeet ovat tehokas työkalu tekstin käsittelyssä ja haun tekemisessä. Ne voivat auttaa sinua löytämään ja muokkaamaan tiettyjä merkkijonoja tai osia merkkijonoista. Rust-ohjelmoinnissa säännöllisiä lausekkeita voi käyttää esimerkiksi datan validoinnissa tai tietokantahakujen tekemisessä.

## Kuinka käyttää säännöllisiä lausekkeita Rust-ohjelmoinnissa?

Ensimmäiseksi on tärkeää tuoda tarvittava liitännäinen `regex` käyttöön Rust-ohjelmassamme. Tämän jälkeen voimme käyttää `Regex`-tyyppiä säännöllisten lausekkeiden luomiseen ja hakuja niiden avulla. Koodiesimerkissä luomme säännöllisen lausekkeen, joka etsii kaikki numerot merkkijonosta ja tulostaa ne näytölle:

```Rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\d+").unwrap();
    let text = "Ostin 3 kiloa omenoita ja 5 kiloa banaaneita.";
    for cap in re.captures_iter(text) {
        println!("{}", &cap[0]);
    }
}

// Output:
// 3
// 5
```

Tässä esimerkissä haluamme löytää kaikki numerot merkkijonosta ja tulostaa ne näytölle. Ensimmäinen rivi luo uuden `Regex`-tyypin ilmentymän, joka sisältää säännöllisen lausekkeen `"\d+"`. Tämä säännöllinen lauseke tarkoittaa "etsi kaikki numerot". Toisessa rivissä määrittelemme merkkijonon, josta haluamme etsiä. Viimeisessä rivissä käytämme `captures_iter`-metodia, joka palauttaa iteratorin, jossa voimme käyttää `cap`-muuttujaa, joka sisältää ensimmäisen löydetyn osuman. Tässä tapauksessa tulostamme osuman itsensä `cap[0]`.

## Syväsukellus säännöllisiin lausekkeisiin

Säännölliset lausekkeet ovat voimakas ja monipuolinen työkalu, jota voi käyttää moniin eri tarkoituksiin. On kuitenkin tärkeää tutustua tarkemmin säännöllisen lausekkeen syntaksiin ja eri mahdollisuuksiin sen käytössä. Joitain hyödyllisiä linkkejä säännöllisiin lausekkeisiin ja niiden käyttöön Rust-ohjelmoinnissa löydät alta.

## Katso myös

- [Rustin regex-dokumentaatio](https://docs.rs/regex/1.5.4/regex/)
- [Regex Coach - interaktiivinen säännöllisten lausekkeiden työkalu](https://weitz.de/regex-coach/)
- [Regex101 - työkalu säännöllisten lausekkeiden testaamiseen ja luomiseen](https://regex101.com/)