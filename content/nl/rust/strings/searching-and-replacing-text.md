---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:08.041509-07:00
description: 'Hoe: .'
lastmod: '2024-03-13T22:44:50.579131-06:00'
model: gpt-4-0125-preview
summary: .
title: Tekst zoeken en vervangen
weight: 10
---

## Hoe:
```Rust
fn main() {
    let text = "Hallo daar!";
    let updated_text = text.replace("daar", "wereld");
    println!("{}", updated_text); // Print "Hallo wereld!"
}
```

Voorbeelduitvoer:
```
Hallo wereld!
```

## Diepgaande duik
Zoeken en vervangen van tekst is er al sinds de eerste teksteditors verschenen. Hulpmiddelen zoals sed in Unix maakten batch-tekstverwerking gemeengoed.

Rust neemt een efficiënte, veilige benadering. De `replace` methode, van het standaardbibliotheek's `str` type, is eenvoudig en wordt gecontroleerd tijdens de compilatietijd.

Alternatieven voor `replace` zijn onder meer regex voor complexe patronen of het itereren van karakters om de vervangingslogica aan te passen.

Onder de motorkap creëert `replace` in Rust een nieuwe `String`, loopt door het origineel, vindt overeenkomsten en construeert vervolgens de nieuwe string met vervangingen. Het gaat goed om met Unicode, wat niet triviaal is.

## Zie ook
- Rust's documentatie over `replace`: https://doc.rust-lang.org/std/primitive.str.html#method.replace
- Regex crate voor meer complexe gebruiksscenario's: https://crates.io/crates/regex
- Sed's handleiding voor historische referentie: https://www.gnu.org/software/sed/manual/sed.html
