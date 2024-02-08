---
title:                "Assosiatiivisten taulukoiden käyttö"
date:                  2024-01-30T19:12:52.362748-07:00
model:                 gpt-4-0125-preview
simple_title:         "Assosiatiivisten taulukoiden käyttö"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Assosiatiiviset taulukot, tai kuten Rustin käyttäjät niitä kutsuvat, "hajautustaulukot", ovat kokoelmia, jotka tallentavat dataa avain-arvo -pareina. Ohjelmoijat käyttävät niitä nopeaan datan etsintään, mikä mahdollistaa tehokkaan datan käsittelyn ainutlaatuisten avainten perusteella.

## Miten:

Rustissa `HashMap`-tyyppi `std::collections`-moduulista tarjoaa assosiatiivisten taulukoiden toiminnallisuuden. Näin voit työskennellä niiden kanssa:

```Rust
use std::collections::HashMap;

fn main() {
    // Uuden HashMapin luominen
    let mut scores = HashMap::new();

    // Arvojen lisääminen
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    // Arvojen hakeminen
    let team_name = String::from("Blue");
    if let Some(score) = scores.get(&team_name) {
        println!("Pisteet tiimille Blue: {}", score); // Tuloste: Pisteet tiimille Blue: 10
    }

    // Arvon päivittäminen
    scores.entry(String::from("Blue")).and_modify(|e| *e += 5);

    // Avain-arvo -parien läpikäynti
    for (key, value) in &scores {
        println!("{}: {}", key, value); // Tuloste: Blue: 15, Yellow: 50
    }
}
```

## Syväsukellus

Rustin `HashMap` käyttää hajautusfunktiota avainten yhdistämiseen arvoihin, mikä mahdollistaa nopean datan noudon. Tämä tehokkuus tulee kuitenkin kustannuksella: hajautustaulukot eivät säilytä elementtiensä järjestystä. Tämä eroaa muiden assosiatiivisten taulukoiden toteutuksista, kuten Pythonin (`dict`) tai Rubyn, jotka viimeisimmissä versioissa säilyttävät lisäysjärjestyksen ominaisuutena. Käyttötarkoituksiin, joissa avain-arvo -parien järjestys on merkittävä, Rustin kehittäjät saattavat harkita `BTreeMap`-käytön `std::collections`-moduulista, joka säilyttää järjestyksen mutta saattaa tarjota hitaamman lisäyksen ja noudon verrattuna `HashMap`iin. Lopulta valinta `HashMap`in ja `BTreeMap`in välillä riippuu tiettyjen vaatimusten mukaan järjestyksen ja suorituskyvyn suhteen.
