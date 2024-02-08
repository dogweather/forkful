---
title:                "Code organiseren in functies"
aliases:
- nl/rust/organizing-code-into-functions.md
date:                  2024-01-28T22:03:18.991405-07:00
model:                 gpt-4-0125-preview
simple_title:         "Code organiseren in functies"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/rust/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Code organiseren in functies gaat over het opsplitsen van je programma in herbruikbare, modulaire stukken die door een naam geïdentificeerd worden. We doen dit om onze code schoner, leesbaarder en makkelijker te debuggen te maken. Het gaat erom dat we onszelf niet herhalen en updates stroomlijnen.

## Hoe:
Stel je voor dat je code hebt die meerdere keren de oppervlakte van een cirkel berekent. In plaats van de formule te herhalen, verpak je het in een functie.

```Rust
fn bereken_cirkel_oppervlakte(radius: f64) -> f64 {
    std::f64::consts::PI * radius.powi(2)
}

fn main() {
    let radius = 5.0;
    let oppervlakte = bereken_cirkel_oppervlakte(radius);
    println!("De oppervlakte van de cirkel is: {}", oppervlakte);
}
```

Output:

```
De oppervlakte van de cirkel is: 78.53981633974483
```

## Diepgaande Duik
Historisch gezien komen functies uit de wiskunde, waar ze inputs naar outputs mappen. In het programmeren zijn ze er al sinds de dagen van assembly, hoewel we ze toen 'subroutines' noemden. Rust-functies kunnen waarden teruggeven en zelfs andere functies dankzij first-class functies en closures.

Alternatieven? Inline code of macro's, maar die zijn rommelig voor complexe logica. Objecten met methoden zijn een andere manier om functionaliteit te organiseren, een andere smaak dan standalone functies.

Implementatie in Rust is vrij eenvoudig. Functies verklaren hun parameter types en retour type. Ze zijn 'snake case' voor naamgeving volgens conventie. Je hebt je publieke functies (`pub fn`) voor gebruik buiten de module en privéfuncties voor intern gebruik. En Rust heeft deze coole functie waar je niet een `return`-trefwoord nodig hebt voor de laatste expressie in een functie.

## Zie Ook
Bekijk deze voor meer info:
- Het Rust Programmeertaal Boek: [Functies](https://doc.rust-lang.org/book/ch03-03-how-functions-work.html)
- Rust door Voorbeeld over [Functies](https://doc.rust-lang.org/rust-by-example/fn.html)
