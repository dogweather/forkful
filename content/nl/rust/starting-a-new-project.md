---
title:                "Een nieuw project starten"
date:                  2024-01-28T22:08:42.552605-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een nieuw project starten"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/rust/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een nieuw project starten in Rust betekent het opzetten van een basisstructuur zodat je code een plek heeft om te leven. Programmeurs starten nieuwe projecten om problemen op te lossen, te leren, of vanaf nul software te ontwikkelen.

## Hoe:

Om een nieuw Rust-project te starten, heb je Cargo nodig - de pakketbeheerder van Rust. Installeer Rust en Cargo via de officiële installatieprogramma, rustup.

```sh
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

Daarna is het een eenvoudig commando om een nieuw project te creëren:

```sh
cargo new my_project
```

Dit commando roept een nieuwe map genaamd 'my_project' op met alle noodzakelijke bestanden:

- `Cargo.toml`: Het manifest van jouw project met metadata en afhankelijkheden.
- `src`: Een map waar jouw bronbestanden wonen.
- `main.rs`: Het belangrijkste toegangspunt voor jouw programma.

Zo eenvoudig ziet je `main.rs` eruit na creatie:

```rust
fn main() {
    println!("Hallo, wereld!");
}
```

Om je project te compileren en uit te voeren:

```sh
cd my_project
cargo run
```

En als bij toverslag zie je de uitvoer:

```
   Compiling my_project v0.1.0 (pad/naar/my_project)
    Finished dev [unoptimized + debuginfo] doel(en) in 0.0 sec
     Running `target/debug/my_project`
Hallo, wereld!
```

## Diepteduik

Rust heeft al vanaf de vroege dagen zijn eigen pakketbeheerder en bouwsysteem, Cargo, gehad. Gecreëerd rond 2013, is het Rust's manier van het beheren van projecten, afhankelijkheden en builds.

Waarom is Cargo zo geweldig voor het starten van nieuwe projecten?

- **Consistentie**: Het creëert een gestandaardiseerde projectstructuur.
- **Afhankelijkheden**: Het beheert externe bibliotheken met gemak.
- **Compilatie**: Het compileert je code, maakt gebruik van Rust's veiligheid en prestatie-eigenschappen.

Andere talen gebruiken verschillende tools - Node.js heeft npm, Ruby heeft Bundler, en Python heeft Pip. Cargo is Rust's antwoord op deze en doet aantoonbaar veel meer 'out-of-the-box' door het bouwsysteem in te sluiten, wat anderen delegeren aan aparte tools, zoals Grunt of Webpack in het JavaScript-ecosysteem.

Alternatieven voor het starten van projecten in Rust? Nou, je zou alles met de hand kunnen maken of IDE's gebruiken, maar waarom het wiel opnieuw uitvinden als Cargo het zware werk doet?

## Zie Ook

- Het Rust Programmeringstaal Boek: https://doc.rust-lang.org/book/
- Rust en Cargo installatiegids: https://www.rust-lang.org/tools/install
- Cargo documentatie: https://doc.rust-lang.org/cargo/
