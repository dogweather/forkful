---
title:                "Konvertere en streng til små bokstaver"
date:                  2024-01-20T17:39:08.912979-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertere en streng til små bokstaver"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å konvertere en streng til små bokstaver betyr å endre alle bokstavene til sine minuskulære motstykker. Programmere gjør dette for å få konsistente data, som forenkler søk og sammenligninger.

## Slik gjør du:
Hvis du har en streng, og du vil gjøre hele greia smått, er det sånn du gjør det i Rust:

```Rust
fn main() {
    let original = "Hei, Norge!";
    let small = original.to_lowercase();
    
    println!("Original: {}", original);
    println!("Smått: {}", small);
}
```

Kjører du dette, får du ut:

```
Original: Hei, Norge!
Smått: hei, norge!
```

Enkelt og greit!

## Dypdykk
I de eldre dagene av programmering, å håndtere ulike kasus i strenger var ofte en kilde til hodebry. Rust bruker Unicode-standard for å konvertere til små bokstaver, som er mye mer omfattende enn ASCII's enkle `A`-til-`a` mapping. Men ikke bekymre deg, `.to_lowercase()` tar høyde for dette. Alternativt, hvis du trenger en rask sjekk kun for ASCII-tegn, kan du bruke `.to_ascii_lowercase()` som ikke vil håndtere særnorske tegn som æ, ø, og å.

Implementasjonen av små bokstaver i Rust er mer kompleks enn bare å bytte ut bokstaver, spesielt når du jobber med Unicode, hvor en bokstav kan transformere til flere små bokstaver, og visse bokstaver kan endres basert på konteksten de brukes i.

## Se Også
- Rust's offisielle dokumentasjon om `.to_lowercase()`: https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase
- Unicode karakter databasen for de dedikerte: https://www.unicode.org/
- For programmeringsprinsipper i Rust: "The Rust Programming Language" boka som er tilgjengelig online på https://doc.rust-lang.org/book/
