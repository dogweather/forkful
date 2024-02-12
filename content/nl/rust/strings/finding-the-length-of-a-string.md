---
title:                "De lengte van een string vinden"
aliases: - /nl/rust/finding-the-length-of-a-string.md
date:                  2024-01-28T22:00:37.664476-07:00
model:                 gpt-4-0125-preview
simple_title:         "De lengte van een string vinden"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/rust/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
De lengte van een string vinden betekent het tellen van het aantal tekens dat het bevat. Programmeurs doen dit om tekstgegevens efficiënt te valideren, formatteren of te verwerken.

## Hoe:
Rust biedt je `len()` voor de directe lengte:

```Rust
fn main() {
    let begroeting = "Hallo, wereld!";
    println!("Lengte: {}", begroeting.len());
}
```

Uitvoer: `Lengte: 13`

Maar let op, `len()` telt bytes, geen tekens. Voor het tellen van tekens, gebruik `.chars().count()`:

```Rust
fn main() {
    let begroeting = "¡Hola, mundo!";
    println!("Aantal tekens: {}", begroeting.chars().count());
}
```

Uitvoer: `Aantal tekens: 12`

## Diepere Duik
`len()` telt bytes omdat Rust-strings UTF-8 gecodeerd zijn. Historisch gezien gebruikten vroege computers ASCII, waarbij elk teken met één byte werd gerepresenteerd. UTF-8 ondersteunt echter een enorm scala aan tekens, waarbij 1 tot 4 bytes per teken worden gebruikt.

Wanneer je `len()` aanroept, telt Rust de bytes in een string, wat snel is maar niet altijd overeenkomt met het aantal tekens. Emoji's of bepaalde accenttekens nemen bijvoorbeeld meer dan één byte in beslag. Daarom is `.chars().count()` belangrijk - het itereert over de tekens en geeft de Unicode scalarwaarde telling, wat het eigenlijke aantal tekens is wat de meeste mensen verwachten.

Wat betreft alternatieven, `.chars().count()` is nauwkeurig maar traag voor lange strings omdat het door elk teken moet itereren. Als prestatie cruciaal is, en je zeker weet dat je te maken hebt met ASCII of Unicode tekens met vaste breedte, is `len()` efficiënter.

Ten slotte, onthoud dat Rusts string-indexering geen directe toegang per tekenpositie toelaat vanwege de werking van UTF-8 codering. Rust voorkomt operaties die per ongeluk strings op ongeldige punten kunnen breken of opsplitsen, wat mogelijk geen volledige tekens vertegenwoordigt.

## Zie Ook
- Rusts officiële string-documentatie: [https://doc.rust-lang.org/std/string/](https://doc.rust-lang.org/std/string/)
- Het Rust Boek over strings: [https://doc.rust-lang.org/book/ch08-02-strings.html](https://doc.rust-lang.org/book/ch08-02-strings.html)
- Om UTF-8 versus ASCII verder te begrijpen, bekijk [https://tools.ietf.org/html/rfc3629](https://tools.ietf.org/html/rfc3629)
