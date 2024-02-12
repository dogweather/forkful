---
title:                "Het gebruik van een interactieve shell (REPL)"
aliases:
- /nl/rust/using-an-interactive-shell-repl.md
date:                  2024-01-28T22:09:30.755852-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het gebruik van een interactieve shell (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/rust/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een interactieve Rust-shell, of REPL (Read-Eval-Print Loop), laat je Rust-code direct uitvoeren, met onmiddellijke resultaten, perfect voor experimenteren of leren. Programmeurs gebruiken het om codefragmenten te testen, te debuggen of gewoon met taalfuncties te spelen zonder de overhead van het compileren van een volledig project.

## Hoe te gebruiken:
Momenteel heeft Rust geen officiële REPL die ermee wordt geleverd. Je kunt externe tools gebruiken zoals `evcxr_repl`. Installeer het met Cargo:

```sh
cargo install evcxr_repl
```

Voer vervolgens de REPL uit:

```sh
evcxr
```

Binnenin, test wat Rust-code:

```rust
let x = 5;
let y = 3;
println!("{} + {} = {}", x, y, x + y);
```

De uitvoer zou moeten zijn:

```
5 + 3 = 8
```

## Diepere Duik
De ethos van Rust is gecentreerd rond veiligheid en prestatie, welke gewoonlijk geassocieerd worden met talen die vooraf gecompileerd worden, en minder met geïnterpreteerde, REPL-vriendelijke talen. Historisch gezien legden talen zoals Python of Ruby de nadruk op het hebben van een REPL voor onmiddellijke feedback, maar waren niet ontworpen met systeemniveau taken in gedachten.

Ondanks de afwezigheid van een officiële REPL in Rust, zijn er een paar alternatieven zoals `evcxr_repl` opgedoken. Deze projecten hacken Rust niet zomaar in een REPL; ze weven slim de compileer-en-draai cyclus van de taal samen in een interactieve sessie. De REPL compileert de code achter de schermen en draait het binaire bestand, waarbij de uitvoer wordt vastgelegd. Op deze manier behoudt het de prestatievoordelen van Rust en biedt het toch die interactieve ervaring.

Er is een lopende discussie in de Rust-gemeenschap over officiële REPL-ondersteuning, en met elke taaliteratie zien we meer verfijning in gereedschap dat uiteindelijk zou kunnen leiden tot een native oplossing.

## Zie Ook
Voor meer info en andere hulpmiddelen:
- Evcxr REPL GitHub repo: [https://github.com/google/evcxr](https://github.com/google/evcxr)
- Rust Playground, een online manier om Rust-code te testen: [https://play.rust-lang.org/](https://play.rust-lang.org/)
- Discussie over REPL-functie in Rust-taal: [https://internals.rust-lang.org/](https://internals.rust-lang.org/)
