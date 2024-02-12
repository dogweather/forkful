---
title:                "Bruke associative tabeller"
aliases: - /no/rust/using-associative-arrays.md
date:                  2024-01-30T19:12:58.916866-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke associative tabeller"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Assosiative tabeller, eller det Rust-utviklere kaller "hash maps", er samlinger som lagrer data i nøkkel-verdi par. Programmerere bruker dem til rask dataoppslag, noe som muliggjør effektiv datahåndtering basert på unike nøkler.

## Hvordan:

I Rust gir `HashMap`-typen fra `std::collections`-modulen funksjonaliteten til assosiative tabeller. Her er hvordan du kan jobbe med dem:

```Rust
use std::collections::HashMap;

fn main() {
    // Opprette en ny HashMap
    let mut scores = HashMap::new();

    // Sette inn verdier
    scores.insert(String::from("Blå"), 10);
    scores.insert(String::from("Gul"), 50);

    // Tilgå verdier
    let team_navn = String::from("Blå");
    if let Some(score) = scores.get(&team_navn) {
        println!("Poeng for team Blå: {}", score); // Utdata: Poeng for team Blå: 10
    }

    // Oppdatere en verdi
    scores.entry(String::from("Blå")).and_modify(|e| *e += 5);

    // Iterere over nøkkel-verdi par
    for (nøkkel, verdi) in &scores {
        println!("{}: {}", nøkkel, verdi); // Utdata: Blå: 15, Gul: 50
    }
}
```

## Dypdykk

`HashMap` i Rust bruker en hash-funksjon for å kartlegge nøkler til verdier, noe som muliggjør rask datahenting. Imidlertid kommer denne effektiviteten med en kostnad: hash maps opprettholder ikke rekkefølgen av elementene sine. Dette står i kontrast til andre implementeringer av assosiative tabeller, som de i Python (`dict`) eller Ruby, som i de nyere versjonene opprettholder rekkefølgen av innskudd som en funksjon. For brukstilfeller der rekkefølgen av nøkkel-verdi par er betydelig, kan Rust-utviklere vurdere å bruke `BTreeMap` fra `std::collections`-modulen, som opprettholder rekkefølgen, men kan tilby langsommere innsetting og henting sammenlignet med `HashMap`. Til syvende og sist avhenger valget mellom `HashMap` og `BTreeMap` av spesifikke krav rundt ordning og ytelse.
