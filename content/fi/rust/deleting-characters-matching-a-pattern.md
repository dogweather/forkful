---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Hahmojen poistaminen määrätyllä kuvioilla tarkoittaa joukkoa ohjelmointitekniikoita, joita käytetään tietyn kriteerin täyttävien merkkijonojen tunnustamiseen ja poistamiseen. Ohjelmoijat tekevät tämän puhdistaakseen tai muokatakseen dataa.

## Näin se toimii:

Käytämme Rustin sisäänrakennettua `replace`-funktiota näissä esimerkeissä. Seuraava koodi poistaa kaikki "a"-kirjaimet merkkijonosta:

```Rust
let s = "Tämä on testi".to_string();
let result = s.replace("a", "");
println!("{}", result);
```

Tuloste on:

```
"Tämä on testi"
```

Voimme myös käyttää säännöllisiä lausekkeita poistamaan määrätyt kuviot. Katso seuraava koodi:

```Rust
use regex::Regex;

let re = Regex::new("a").unwrap();
let s = "Tämä on toinen testi".to_string();
let result = re.replace_all(&s, "");
println!("{}", result);
```
Tuloste on:

```
"Tämä on toinen testi"
```

## Syvä sukellus

Historiallisesti merkkijonojen poistaminen määrätyllä kuvioilla on ollut yleinen tehtävä ohjelmoijille. Se on tapa virheenkorjaukselle, datan puhdistukselle ja tiedon erottamiselle.
Rustissa, `replace`- ja `regex`-kirjastojen avulla tämä on suhteellisen yksinkertaista. `regex`-kirjasto on erittäin tehokas, mutta se voi olla hidas suurilla datamassoilla. Joissakin tapauksissa voi olla nopeampaa käyttää muita menetelmiä, kuten `str::replacen`.

Rustissa on monia tapoja toteuttaa hahmojen poistaminen määrätyillä kuvioilla. `replace`-funktion lisäksi voit käyttää `regex`-kirjastoa, joka antaa sinulle monipuolisen ja voimakkaan suodatinmekanismin kuvioiden poistamiseen.

## Katso myös

- Rustin virallinen dokumentaatio: https://doc.rust-lang.org/std/string/struct.String.html#method.replace
- RegEx-kirjaston dokumentaatio: https://docs.rs/regex/1.3.9/regex/