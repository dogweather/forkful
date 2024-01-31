---
title:                "Att använda associativa arrayer"
date:                  2024-01-30T19:12:54.611767-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda associativa arrayer"

category:             "Rust"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Associativa arrayer, eller vad Rustanvändare kallar "hash maps", är samlingar som lagrar data i nyckel-värde-par. Programmerare använder dem för snabb uppslagning av data, vilket möjliggör effektiv datahantering baserad på unika nycklar.

## Hur man gör:

I Rust tillhandahåller typen `HashMap` från modulen `std::collections` funktionerna för associativa arrayer. Så här kan du arbeta med dem:

```Rust
use std::collections::HashMap;

fn main() {
    // Skapa en ny HashMap
    let mut scores = HashMap::new();

    // Infoga värden
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    // Tillgå värden
    let team_name = String::from("Blue");
    if let Some(score) = scores.get(&team_name) {
        println!("Poäng för lag Blue: {}", score); // Utmatning: Poäng för lag Blue: 10
    }

    // Uppdatera ett värde
    scores.entry(String::from("Blue")).and_modify(|e| *e += 5);

    // Iterera över nyckel-värde-par
    for (key, value) in &scores {
        println!("{}: {}", key, value); // Utmatning: Blue: 15, Yellow: 50
    }
}
```

## Djupdykning

`HashMap` i Rust använder en hashfunktion för att mappa nycklar till värden, vilket möjliggör snabb upphämtning av data. Dock kommer denna effektivitet med en kostnad: hash maps bibehåller inte ordningen på sina element. Detta är i kontrast till andra implementationer av associativa arrayer, som de i Python (`dict`) eller Ruby, som i senare versioner bibehåller insättningsordning som en funktion. För användningsfall där ordningen på nyckel-värde-par är betydande, kan Rustutvecklare överväga att använda `BTreeMap` från modulen `std::collections`, som bibehåller ordning men kan erbjuda långsammare insättning och upphämtning jämfört med `HashMap`. I slutändan beror valet mellan `HashMap` och `BTreeMap` på specifika krav gällande ordning och prestanda.
