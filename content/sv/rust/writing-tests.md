---
title:                "Skriva tester"
html_title:           "Rust: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av att skriva högkvalitativ kod. Det hjälper dig att säkerställa att din kod fungerar korrekt och förhindrar potentiella buggar och fel från att nå produktionsmiljön.

## Så här

För att börja skriva tester i Rust behöver du först inkludera biblioteket "test" i din kod. Sedan kan du skapa en funktion som innehåller testkod och använder makron för att jämföra resultatet med det förväntade värdet. Här är ett exempel som testar en enkel funktion som adderar två tal:

```rust
use test::Bencher;

fn add(x: u32, y: u32) -> u32 {
    x + y
}

#[test]
fn test_add_two_numbers() {
    assert_eq!(add(2, 3), 5);
}

fn main() {
    println!("{}", add(2, 3));
}
```

När du kör denna kod med hjälp av "cargo test" kommer testet att köras och du kommer få en rapport om det lyckades eller misslyckades. Om allting fungerar som det ska, kommer funktionen "test_add_two_numbers" att lyckas och du kommer att se "test successful" i din terminal. Om något går fel, kommer du att få en detaljerad rapport om vad som gick fel.

## Deep Dive

När du skriver tester i Rust är det viktigt att förstå hur makron som "assert_eq!" fungerar. Dessa makron jämför de två värdena (förväntat värdet och det faktiska värdet) och ger dig en rapport om de är lika. Om de inte är det, kommer det att rapportera vilket av dem som avvek från det förväntade värdet. Detta hjälper dig att snabbt identifiera och åtgärda eventuella problem i din kod.

En annan viktig aspekt av att skriva tester är att täcka så många fall som möjligt. Det är viktigt att inte bara testa positiva fall där koden fungerar som den ska, utan också testa negativa fall där koden förväntas krascha eller ge felaktiga resultat. Detta hjälper till att upptäcka och åtgärda buggar som annars skulle kunna bli stora problem i produktionen.

## Se också

- [Official Rust documentation on testing](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [The Rust Book](https://doc.rust-lang.org/book/) - en omfattande guide för Rust-programmering
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/) - en interaktiv guide för att lära sig Rust genom kodexempel
- [Awesome Rust](https://github.com/rust-unofficial/awesome-rust) - en samling av olika verktyg, bibliotek och resurser för Rust-programmering