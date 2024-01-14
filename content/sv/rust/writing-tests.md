---
title:                "Rust: Skriva tester"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Varför
Att skriva tester är en viktig del av programmeringsprocessen och kan hjälpa till att upptäcka buggar och fel i koden innan de når användarna. Med Rusts inbyggda testfunktioner kan du enkelt skapa och köra tester för din kod.

## Hur man skriver tester i Rust
Det första steget för att skriva tester i Rust är att inkludera `test` biblioteket som en dependecy i ditt projekt. Sedan kan du använda den inbyggda `#[test]` makron för att ange en funktion som ett test.

```Rust
#[test]
fn test_sum() {
    let result = sum(1, 2);
    assert_eq!(result, 3);
}
```

För att köra testerna kan du använda kommandot `cargo test`. Om alla tester passerar kommer du att få följande utmatning:

``` 
running 1 test
test test_sum ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```
Om en eller flera tester misslyckas kommer du att få en uppdaterad utmatning som visar vilka tester som har misslyckats och vad det förväntade resultatet var.

## Djupdykning
Rust har en mängd olika assert makron som kan användas för att skriva tester. Det finns också möjlighet att använda `#[should_panic]` för att testa om en funktion korrekt kraschar eller inte.

En annan viktig punkt att tänka på när du skriver tester är att ha en god kodtäckning. Det innebär att alla delar av koden ska testas för att säkerställa att inga buggar slipper igenom. För att kontrollera kodtäckningen i Rust kan du använda verktyget `cargo tarpaulin`.

## Se också
- [Rust vägledning om tester](https://doc.rust-lang.org/rust-wasm/book/game-of-life/testing.html)
- [Rust API referens för test](https://doc.rust-lang.org/std/macro.assert.html)
- [Cargo tarpaulin dokumentation](https://github.com/xd009642/tarpaulin)