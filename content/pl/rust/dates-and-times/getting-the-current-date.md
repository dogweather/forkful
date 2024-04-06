---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:50.754375-07:00
description: "Jak to zrobi\u0107: Biblioteka standardowa Rusta oferuje ograniczon\u0105\
  , ale szybk\u0105 metod\u0119 uzyskania bie\u017C\u0105cego czasu, cho\u0107 nie\
  \ bezpo\u015Brednio bie\u017C\u0105cej daty w\u2026"
lastmod: '2024-03-13T22:44:35.196037-06:00'
model: gpt-4-0125-preview
summary: "Biblioteka standardowa Rusta oferuje ograniczon\u0105, ale szybk\u0105 metod\u0119\
  \ uzyskania bie\u017C\u0105cego czasu, cho\u0107 nie bezpo\u015Brednio bie\u017C\
  \u0105cej daty w formacie kalendarzowym."
title: Pobieranie aktualnej daty
weight: 29
---

## Jak to zrobić:


### Korzystając z biblioteki standardowej Rust
Biblioteka standardowa Rusta oferuje ograniczoną, ale szybką metodę uzyskania bieżącego czasu, choć nie bezpośrednio bieżącej daty w formacie kalendarzowym. Oto jak to zrobić:

```rust
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => println!("Aktualny czas: {} sekund od epoki Unix.", n.as_secs()),
        Err(_) => panic!("SystemTime przed epoką Unix!"),
    }
}
```

Wyjście:
```
Aktualny czas: 1615390665 sekund od epoki Unix.
```

### Korzystając z biblioteki Chrono
Dla bardziej kompleksowej funkcjonalności daty i czasu, włącznie z pobieraniem bieżącej daty, powinieneś użyć biblioteki `chrono`. Najpierw dodaj `chrono` do swojego `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Następnie możesz użyć `chrono`, aby uzyskać bieżącą datę:

```rust
extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let now = Local::now();
    println!("Aktualna data: {}-{}-{}", now.year(), now.month(), now.day());
}
```

Wyjście:
```
Aktualna data: 2023-4-20
```

Biblioteka `chrono` ułatwia pracę z datami i czasami, oferując szeroki zakres funkcjonalności wykraczających poza samo pobieranie bieżącej daty, w tym parsowanie, formatowanie i operacje arytmetyczne na datach i czasach.
