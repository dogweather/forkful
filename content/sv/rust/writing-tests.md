---
title:                "Rust: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av programmering, oavsett vilket språk man använder. Med hjälp av tester kan man säkerställa att ens kod fungerar som den ska och undvika buggar och felaktigheter. Det kan även bidra till en bättre och mer strukturerad kodbas.

## Hur man gör det

Att skriva tester i Rust är en relativt enkel process. Man använder en inbyggd funktion, `assert_eq!`, för att kontrollera om två värden är lika. Nedan finns ett exempel där vi testar en funktion som adderar två tal:

```Rust
fn add(x: i32, y: i32) -> i32 {
    return x + y;
}

#[test]
fn test_add() {
    let sum = add(3, 5);
    assert_eq!(sum, 8);
}
```

När vi kör detta test med hjälp av kommandot `cargo test` kommer det att ge oss en output som säger att testet har passerat. Om vi ändrar värdet på `sum` till ett annat tal som inte stämmer överens med vår förväntade output, kommer testet att misslyckas.

## Djupdykning

Det finns olika typer av tester man kan skriva i Rust, såsom enhetstester, integrationstester och funktionella tester. Det är viktigt att välja rätt typ av test för den specifika koden man vill testa.

En annan viktig aspekt av att skriva tester är att skriva testbar kod. Detta innebär att koden bör vara strukturerad på ett sätt som gör den enkel att testa. Det kan innebära att dela upp koden i mindre funktioner eller använda sig av designmönster som underlättar testning.

Genom att skriva tester för sin kod kan man också få en bättre förståelse för hur den fungerar och undvika potentiella buggar och fel i framtiden.

## Se även

Här är några länkar som kan vara till hjälp för att lära sig mer om att skriva tester i Rust:

- [The Rust Book - Testing](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Writing Tests in Rust - Dev.to](https://dev.to/anshulgupta1029/writing-tests-in-rust-2fih)
- [Rust Test Macro - Rust Documentation](https://doc.rust-lang.org/rust-by-example/testing/unit_testing.html)