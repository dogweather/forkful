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

## Vad & Varför?
Att skriva tester är en vanlig praxis inom programmering för att säkerställa att koden fungerar som den ska. Tester är små bitar av kod som kontrollerar att olika delar av programmet uppfyller förväntade krav och ger ökad kvalitet och säkerhet i koden.

## Hur man:
Det är enkelt att skriva tester i Rust med hjälp av det inbyggda ramverket för enhetstester. Man börjar med att importera ```std::test``` biblioteket och sedan definiera en testfunktion som använder makrot ```assert``` för att kontrollera att uttryck eller funktioner ger förväntat resultat. Nedan finns ett exempel på en testfunktion för en enkel funktion som summerar två tal:

```Rust
use std::test;

fn summera(tal1: i32, tal2: i32) -> i32 {
  return tal1 + tal2;
}

#[test]
fn test_summera() {
  assert_eq!(summera(2, 3), 5);
}
```

När man kör testerna med kommandot ```cargo test``` kommer programmet att gå igenom alla testfunktioner och rapportera om några av dem misslyckats.

## Djupdykning:
Att skriva tester har blivit allt mer populärt inom programmering de senaste åren och anses vara ett viktigt steg i att skapa pålitlig och robust kod. Innan enhetstester blev populära, användes manuella tester där en person skulle köra igenom programmet och kontrollera att allt fungerade som det skulle. Detta var en tidskrävande och ineffektiv process jämfört med automatiserade tester. Alternativet till enhetstester är integrationstester som kontrollerar att flera delar av programmet fungerar bra tillsammans men är mer komplexa och tar längre tid att skriva.

Det är också möjligt att testa privata funktioner och metoder genom att använda makrot ```#[test]``` tillsammans med ```#[allow(private)]``` för att tillåta åtkomst till dessa privata delar av koden. Detta gör det möjligt att testa mer komplexa och interna delar av programmet utan att behöva skriva om koden för att göra dem tillgängliga för externa tester.

## Se även:
För mer information om hur man skriver tester i Rust, se följande länkar:
- [Rust Book - Writing Automated Tests](https://doc.rust-lang.org/book/ch11-00-testing.html)