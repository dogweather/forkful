---
title:                "Substrings extraheren"
aliases:
- /nl/rust/extracting-substrings/
date:                  2024-01-28T22:00:12.574190-07:00
model:                 gpt-4-0125-preview
simple_title:         "Substrings extraheren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/rust/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Substrings extraheren betekent een kleiner stuk uit een grotere string halen—een beetje zoals het snijden van een cake om het stuk te krijgen dat je echt wilt eten. Programmeurs doen dit om gegevens te plukken, tekst te parseren, of simpelweg informatie op te breken voor eenvoudiger hantering.

## Hoe te:

Laten we onze handen vies maken met Rust. Stel je voor dat je een string hebt en je wilt een specifiek deel ervan. Je kunt slicing gebruiken `&str[start..end]` waar `start` is waar je begint, en `end` is waar je stopt.

```Rust
fn main() {
    let text = "The quick brown fox jumps over the lazy dog";
    let quick_brown = &text[4..15]; // Sliced van 4e tot 14e index
    println!("{}", quick_brown); // Output: quick brown
}
```

Slicing is netjes, maar het kan leiden tot panics als je indices niet vallen op tekenlimieten. Om dit te voorkomen, biedt Rust methoden zoals `get`:

```Rust
fn main() {
    let text = "The quick brown fox";
    match text.get(4..15) {
        Some(substring) => println!("{}", substring), // veilige slicing
        None => println!("Slice ligt buiten de grenzen."),
    }
}

// Output: quick brown
```

Daar heb je het—een snelle blik op substringextractie in Rust. Kijk hoe makkelijk dat was!

## Diepere Duik

Slicing in talen met UTF-8 gecodeerde strings zoals Rust is een beetje lastig—karakters kunnen meer dan één byte zijn! Voor Rust, in talen zoals C, kon string handling een met bugs bezaaide hoofdpijn zijn, aangezien je handmatig geheugen beheerde.

Rust's `str` type is een reeks UTF-8 bytes, altijd geldig UTF-8. Het veilig extraheren van substrings respecteert deze tekenlimieten.

Alternatieven voor slicing omvatten het gebruik van iterators of regex voor meer complexe patronen, maar die komen met overhead. Bij slicing controleert Rust of byte-indexen overeenkomen met karaktergrenzen op runtime, mogelijke crashes voorkomend door ongeldige slices.

## Zie Ook

- Rust Boek over strings: https://doc.rust-lang.org/book/ch08-02-strings.html
- Rust voorbeeld – Strings: https://doc.rust-lang.org/rust-by-example/std/str.html
- Rust documentatie voor `str`: https://doc.rust-lang.org/std/primitive.str.html
