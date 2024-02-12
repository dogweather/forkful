---
title:                "Verwendung von assoziativen Arrays"
aliases: - /de/rust/using-associative-arrays.md
date:                  2024-01-30T19:12:47.346598-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verwendung von assoziativen Arrays"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Assoziative Arrays oder, wie Rustaceaner sie nennen, "Hash Maps", sind Sammlungen, die Daten in Schlüssel-Wert-Paaren speichern. Programmierer nutzen sie für schnelle Datensuche, was eine effiziente Datenmanipulation basierend auf einzigartigen Schlüsseln ermöglicht.

## Wie geht das:

In Rust bietet der Typ `HashMap` aus dem Modul `std::collections` die Funktionalität von assoziativen Arrays. Hier ist, wie Sie damit arbeiten können:

```Rust
use std::collections::HashMap;

fn main() {
    // Erstellen einer neuen HashMap
    let mut scores = HashMap::new();

    // Werte einfügen
    scores.insert(String::from("Blau"), 10);
    scores.insert(String::from("Gelb"), 50);

    // Werte zugreifen
    let team_name = String::from("Blau");
    if let Some(score) = scores.get(&team_name) {
        println!("Punktzahl für Team Blau: {}", score); // Ausgabe: Punktzahl für Team Blau: 10
    }

    // Einen Wert aktualisieren
    scores.entry(String::from("Blau")).and_modify(|e| *e += 5);

    // Über Schlüssel-Wert-Paare iterieren
    for (key, value) in &scores {
        println!("{}: {}", key, value); // Ausgabe: Blau: 15, Gelb: 50
    }
}
```

## Tiefer eintauchen

Die `HashMap` in Rust verwendet eine Hashing-Funktion, um Schlüssel auf Werte abzubilden, was eine schnelle Datenabrufung ermöglicht. Diese Effizienz hat jedoch ihren Preis: Hash-Maps halten die Reihenfolge ihrer Elemente nicht aufrecht. Dies steht im Gegensatz zu anderen Implementierungen assoziativer Arrays, wie denen in Python (`dict`) oder Ruby, die in neueren Versionen die Einfügereihenfolge als Merkmal beibehalten. Für Anwendungsfälle, bei denen die Reihenfolge der Schlüssel-Wert-Paare signifikant ist, könnten Rust-Entwickler in Erwägung ziehen, die `BTreeMap` aus dem Modul `std::collections` zu verwenden, welche die Reihenfolge beibehält, aber möglicherweise langsamere Einfügung und Abrufung im Vergleich zu `HashMap` bietet. Letztendlich hängt die Wahl zwischen `HashMap` und `BTreeMap` von den spezifischen Anforderungen an die Ordnung und Leistung ab.
