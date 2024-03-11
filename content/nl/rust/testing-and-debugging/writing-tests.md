---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:42.288523-07:00
description: "Tests schrijven betekent codefragmenten maken die controleren of andere\
  \ stukjes code goed werken. Programmeurs doen dit om bugs vroeg te vinden,\u2026"
lastmod: '2024-03-11T00:14:24.415285-06:00'
model: gpt-4-0125-preview
summary: "Tests schrijven betekent codefragmenten maken die controleren of andere\
  \ stukjes code goed werken. Programmeurs doen dit om bugs vroeg te vinden,\u2026"
title: Tests Schrijven
---

{{< edit_this_page >}}

## Wat & Waarom?

Tests schrijven betekent codefragmenten maken die controleren of andere stukjes code goed werken. Programmeurs doen dit om bugs vroeg te vinden, functionaliteit te waarborgen, en toekomstige wijzigingen te voorkomen die dingen in de war schoppen.

## Hoe:

Rust maakt testen eenvoudig. Laten we een functie en een test ervoor schrijven.

De functie:

```Rust
fn add_two(a: i32) -> i32 {
    a + 2
}
```

De test:

```Rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(4, add_two(2));
    }
}
```

Voer tests uit met `cargo test`. Verwachte uitvoer:

```plaintext
   Compiling my_crate v0.1.0 (/path/to/my_crate)
    Finished test [unoptimized + debuginfo] target(s) in 0.31 secs
     Running unittests (target/debug/deps/my_crate-abc123)

running 1 test
test tests::it_adds_two ... ok

test resultaat: ok. 1 geslaagd; 0 gefaald; 0 genegeerd; 0 gemeten; 0 gefilterd; voltooid in 0.00s
```

## Diepere Duik

Historisch gezien worden tests geschreven na de code (post-hoc testing). Rust moedigt aan om tests naast of voor uw code te schrijven (test-gedreven ontwikkeling, TDD). Er zijn andere vormen van testen - integratietests, doc tests, enz. - elk met unieke implementatiedetails.

Tests in Rust worden typisch geschreven in hetzelfde bestand of een `tests/` directory. Ze kunnen unit tests zijn (zoals het `it_adds_two` voorbeeld), integratietests (in aparte bestanden), of documentatietests (ingebed in doc commentaren). De Rust compiler weet dat functies met `#[test]` als tests behandeld moeten worden om met `cargo test` uit te voeren.

## Zie Ook

- Het Rust Boek over testen: https://doc.rust-lang.org/book/ch11-00-testing.html
- Rust voorbeeld's testsectie: https://doc.rust-lang.org/stable/rust-by-example/testing.html
- API richtlijnen over testen: https://rust-lang.github.io/api-guidelines/documentation.html#crate-provides-docs-including-rustdoc-and-tests-c-dox
