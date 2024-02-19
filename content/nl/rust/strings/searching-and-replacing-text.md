---
aliases:
- /nl/rust/searching-and-replacing-text/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:08.041509-07:00
description: "Zoeken en vervangen van tekst is het proces van het vinden van strings\
  \ binnen strings en deze omwisselen voor iets anders. Programmeurs doen dit om\u2026"
lastmod: 2024-02-18 23:09:01.602864
model: gpt-4-0125-preview
summary: "Zoeken en vervangen van tekst is het proces van het vinden van strings binnen\
  \ strings en deze omwisselen voor iets anders. Programmeurs doen dit om\u2026"
title: Tekst zoeken en vervangen
---

{{< edit_this_page >}}

## Wat & Waarom?
Zoeken en vervangen van tekst is het proces van het vinden van strings binnen strings en deze omwisselen voor iets anders. Programmeurs doen dit om gegevens te bewerken, code te herstructureren, of tekstmanipulatie te automatiseren.

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
