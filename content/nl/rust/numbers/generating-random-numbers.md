---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:50.754124-07:00
description: "Het genereren van willekeurige getallen in Rust omvat het gebruik van\
  \ bibliotheken om onvoorspelde numerieke waarden te produceren, wat onmisbaar is\
  \ voor\u2026"
lastmod: 2024-02-19 22:05:09.637843
model: gpt-4-0125-preview
summary: "Het genereren van willekeurige getallen in Rust omvat het gebruik van bibliotheken\
  \ om onvoorspelde numerieke waarden te produceren, wat onmisbaar is voor\u2026"
title: Willekeurige getallen genereren
---

{{< edit_this_page >}}

## Wat & Waarom?

Het genereren van willekeurige getallen in Rust omvat het gebruik van bibliotheken om onvoorspelde numerieke waarden te produceren, wat onmisbaar is voor taken variërend van cryptografie en simulaties tot gaming en gerandomiseerde algoritmes.

## Hoe te:

Rust is afhankelijk van externe crates voor het genereren van willekeurige getallen, waarbij `rand` het meest gebruikt wordt. Om te beginnen met het genereren van willekeurige getallen, moet je eerst `rand` toevoegen aan je `Cargo.toml` bestand:

```toml
[dependencies]
rand = "0.8.5"
```

Vervolgens kun je in je Rust-code willekeurige getallen genereren met `rand`. Hier is een voorbeeld van het genereren van een willekeurig geheel getal en een zwevendekommagetal:

```rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    
    // Genereer een willekeurig geheel getal tussen 1 en 10
    let random_int: i32 = rng.gen_range(1..=10);
    println!("Willekeurig Geheel Getal: {}", random_int);
    
    // Genereer een willekeurig zwevendekommagetal tussen 0.0 en 1.0
    let random_float: f64 = rng.gen::<f64>();
    println!("Willekeurig Zwevendekommagetal: {}", random_float);
}
```

Een mogelijke uitvoer zou kunnen zijn:

```plaintext
Willekeurig Geheel Getal: 7
Willekeurig Zwevendekommagetal: 0.9401077112175732
```

Merk op dat het opnieuw uitvoeren van het programma verschillende waarden zal opleveren.

## Diepere Duik

De generatie van willekeurige getallen in Rust, mogelijk gemaakt door `rand` en zijn afhankelijkheden zoals `getrandom`, vertegenwoordigt een brede abstractie over faciliteiten van het besturingssysteem en algoritmische generatoren. Historisch gezien is willekeurigheid in rekenen geëvolueerd van eenvoudige, voorspelbare algoritmes naar complexe, cryptografisch veilige methoden. Rusts benadering omvat deze evolutie door zijn inplugbare `Rng` trait, welke kan worden ondersteund door verschillende generatoren afhankelijk van de vereiste kwaliteit van willekeurigheid en prestatie.

Voor de meeste toepassingen biedt vertrouwen op `rand` en de RNG van het systeem een goede balans tussen eenvoud en entropie. Echter, voor cryptografische toepassingen, verwijzen crates zoals `rand` naar `getrandom` voor seeding, welke op zichzelf vertrouwt op OS-specifieke mechanismen (bv. `/dev/urandom` op Unix-achtige systemen), wat cryptografisch veilige willekeurigheid verzekert.

Als alternatief, als je specifieke behoeften hebt die niet worden vervuld door `rand`, kan het verkennen van andere crates of het implementeren van aangepaste generatoren gebaseerd op wiskundige modellen een route zijn. Desondanks bieden `rand` en zijn ecosysteem voor het overgrote deel van de gebruikscases robuuste oplossingen die zowel efficiënt als eenvoudig te integreren zijn in Rust-applicaties.
