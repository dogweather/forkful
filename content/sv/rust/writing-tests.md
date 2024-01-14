---
title:    "Rust: Att skriva tester"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av att skriva kod i Rust. Det hjälper till att upptäcka buggar och säkerställa att koden fungerar som det är tänkt. Dessutom kan det spara tid i det långa loppet genom att undvika fel i produktion.

## Hur man gör det

Att skriva tester i Rust är enkelt och effektivt. För att skapa en test fil, börja med att importera "test" biblioteket:

```Rust
use std::test;
```

Sedan kan du definiera dina tester med funktionen "test::assert". Till exempel, om du vill testa en funktion som adderar två tal och returnerar summan, kan du skriva följande:

```Rust
fn add(x: i32, y: i32) -> i32 {
    x + y
}

#[test]
fn test_add() {
    assert_eq!(add(3, 5), 8);
}
```

Här använder vi "assert_eq" för att jämföra resultatet av funktionen med det förväntade värdet. Om det inte stämmer, kommer testet att misslyckas och ge dig en information om var felet ligger.

## Djupdykning

När du skriver tester, är det viktigt att täcka alla möjliga fall av din kod. Rust har olika funktioner som hjälper dig att göra det, som t.ex. "assert_ne" för att testa om det förväntade värdet är olik det faktiska, och "assert!(condition)" för att testa om en villkor är sant.

Det är också bra att ha flera tester för en funktion för att se hur den fungerar med olika parametrar och gränsvärden. Detta kan hjälpa till att upptäcka och åtgärda buggar tidigt.

Slutligen, var noga med att skriva testet i en separat fil och inte blanda dem med din faktiska kod. Detta håller din kodbas ren och organiserad.

## Se även

- [The Rust Book: Writing tests](https://doc.rust-lang.org/book/testing.html)
- [Rust by Example: Testing](https://rustbyexample.com/testing.html)
- [Rust test Crate Documentation](https://doc.rust-lang.org/std/test/index.html)