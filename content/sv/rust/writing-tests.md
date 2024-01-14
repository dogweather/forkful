---
title:    "Rust: Att skriva tester"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Varför

Det finns många olika programmeringsspråk där ute, men varför ska man välja Rust? En av anledningarna är att Rust har inbyggd stöd för enhetstester, vilket är en viktig del av att skapa robusta och pålitliga program. Om du vill utveckla högkvalitativa program är det viktigt att inkludera tester i din kod.

## Hur man skriver tester i Rust

För att skriva tester i Rust använder man sig av modulen `#[cfg(test)]` tillsammans med `#[test]` för att markera funktioner som tester. Detta gör det möjligt att köra testerna endast när programmets flagga för testmiljö är satt. Ett enkelt exempel kan se ut så här:

```Rust
#[cfg(test)]
mod tests {
    #[test]
    fn test_addition() {
        assert_eq!(2 + 2, 4);
    }
}
```

Här har vi en testfunktion som kontrollerar att en enkel addition ger det förväntade resultatet. `#[test]` markerar funktionen som ett test och `assert_eq!` används för att jämföra det faktiska resultatet med det förväntade. Om de inte är lika kommer testet att misslyckas och du får en varning.

Testerna kan sedan köras via kommandot `cargo test` och du kommer få en lista över resultatet från alla tester som körts.

## Djupdykning i tester

Att skriva tester är en viktig del av utvecklingen i Rust, men det finns också potential att skapa mer komplexa tester. Ett annat användbart verktyg för testning är `#[should_panic]` som kan användas för att testa att en funktion faktiskt resulterar i ett avbrott. Man kan också skapa egna tester genom att använda makron från testmodulen.

Det finns också andra moduler som kan hjälpa till att skriva tester i Rust, såsom `mockall` för att skapa mockning och `mutagen` för att testa felaktiga dataflöden.

## Se även

Här är några länkar för vidare läsning om enhetstester i Rust:

- [Rust dokumentation för tester](https://doc.rust-lang.org/book/ch11-01-writing-tests.html)
- [Test module](https://doc.rust-lang.org/rust-by-example/testing/unit_testing.html)
- [Mockall för Rust](https://crates.io/crates/mockall)
- [Mutagen för Rust](https://crates.io/crates/mutagen)

Lycka till med att skriva tester i Rust!