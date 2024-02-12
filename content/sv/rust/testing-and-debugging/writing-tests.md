---
title:                "Skriva tester"
aliases:
- /sv/rust/writing-tests.md
date:                  2024-02-03T19:32:49.814664-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriva tester"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva tester i Rust innebär att skapa automatiserade kontroller för att säkerställa att din kod fungerar som förväntat. Programmerare gör detta för att upptäcka buggar tidigt, underlätta refaktorering och bibehålla kodkvaliteten över tid.

## Hur man gör:

Rusts inbyggda testramverk stöder enhetstester, integrationstester och dokumentationstester utan behov av externa bibliotek. Tester markeras med `#[test]`, och varje funktion som är markerad så kompileras som ett test.

### Skriva ett enhetstest:

Placera enhetstester i modulen de testar med hjälp av en `tests`-submodul märkt med `#[cfg(test)]` för att säkerställa att de endast kompileras när du testar.

```rust
// lib.rs eller main.rs
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(add(2, 2), 4);
    }
}
```

Köra tester:
```shell
$ cargo test
```

Utdata:
```shell
   Compiling your_package_name v0.1.0 (/path/to/your_package)
    Finished test [unoptimized + debuginfo] target(s) in 0.00 secs
     Running unittests src/lib.rs (eller src/main.rs)

running 1 test
test tests::it_adds_two ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

### Skriva integrationstester:

Integrationstester placeras i en tests-katalog på högsta nivån av ditt projekt, bredvid `src`. Varje `.rs`-fil i `tests` kompileras som sin egen separata skräp.

```rust
// tests/integration_test.rs
use your_package_name;

#[test]
fn it_adds_two() {
    assert_eq!(your_package_name::add(2, 2), 4);
}
```

### Testa med populära tredjepartbibliotek:

För mer omfattande testmöjligheter kan `proptest`-biblioteket generera en bred uppsättning indata för att testa funktioner.

Lägg till `proptest` som en utvecklingsberoende i `Cargo.toml`:

```toml
[dev-dependencies]
proptest = "1.0"
```

Använd `proptest` för att köra samma test med många automatiskt genererade indata:

```rust
// inuti tests/integration_test.rs eller en moduls #[cfg(test)]

use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash(a: i32, b:i32) {
        your_package_name::add(a, b);
    }
}
```

Detta kontrollerar att `add` inte kraschar för ett stort spann av `i32`-indata.
