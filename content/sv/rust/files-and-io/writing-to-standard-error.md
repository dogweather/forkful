---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:31.872891-07:00
description: "Att skriva till standardfel (stderr) i Rust handlar om att dirigera\
  \ felmeddelanden och diagnostik till konsolen separat fr\xE5n standardutdatan (stdout).\u2026"
lastmod: 2024-02-19 22:04:56.919964
model: gpt-4-0125-preview
summary: "Att skriva till standardfel (stderr) i Rust handlar om att dirigera felmeddelanden\
  \ och diagnostik till konsolen separat fr\xE5n standardutdatan (stdout).\u2026"
title: Skriva till standardfel
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva till standardfel (stderr) i Rust handlar om att dirigera felmeddelanden och diagnostik till konsolen separat från standardutdatan (stdout). Programmerare gör detta för att skilja vanlig programutdata från felmeddelanden, vilket gör det enklare att hantera fel på lämpligt sätt eller omdirigera dem till loggar eller filer under körning.

## Hur man gör:
Rust erbjuder ett rakt på sak sätt att skriva till stderr med hjälp av `eprintln!`-makrot, liknande hur `println!` används för stdout. Här är ett grundläggande exempel:

```rust
fn main() {
    eprintln!("Det här är ett felmeddelande!");
}
```

Exempelutdata (till standardfel):
```
Det här är ett felmeddelande!
```

För mer kontroll över felmeddelandena, som när du vill formatera text eller hantera I/O-resultat, använd funktionen `stderr` från modulen `std::io`. Denna metod ger en hantering till den globala stderr-strömmen, som du sedan kan skriva till med metoder som `write_all` eller `writeln` från `Write`-egenskapen:

```rust
use std::io::{self, Write};

fn main() {
    let stderr = io::stderr();
    let mut handle = stderr.lock();
    
    writeln!(handle, "Formaterat felmeddelande: {}", 404).expect("Misslyckades med att skriva till stderr");
}
```

Exempelutdata (till standardfel):
```
Formaterat felmeddelande: 404
```

Om du arbetar i miljöer eller applikationer där du litar på bibliotek för loggning eller felhantering, så är bibliotek som `log` och `env_logger` populära. Även om de används mer för loggningssyften, är de konfigurerbara och kan dirigera felloggnivåer till stderr. Nedan finns ett enkelt användningsexempel med `log` och `env_logger`:

Först, lägg till beroendena i din `Cargo.toml`:
```toml
[dependencies]
log = "0.4"
env_logger = "0.9"
```

Sedan, ställ in och använd loggningen i din applikation:
```rust
fn main() {
    env_logger::init();
    log::error!("Det här är ett felmeddelande loggat till stderr");
}
```

När du kör detta program (efter att ha ställt in `env_logger` med en lämplig miljövariabel, till exempel, `RUST_LOG=error`) kommer felmeddelandet att skrivas ut till stderr, genom att använda loggningsinfrastrukturen.

```plaintext
ERROR: Det här är ett felmeddelande loggat till stderr
```
