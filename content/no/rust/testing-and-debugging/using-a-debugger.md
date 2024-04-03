---
date: 2024-01-26 04:10:23.734989-07:00
description: "Rust st\xF8tter forskjellige debuggere, men en vanlig en er `gdb` for\
  \ GNU/Linux eller `lldb` for macOS. Du kan ogs\xE5 bruke `rust-gdb` eller `rust-lldb`,\
  \ som\u2026"
lastmod: '2024-03-13T22:44:40.578387-06:00'
model: gpt-4-0125-preview
summary: "Rust st\xF8tter forskjellige debuggere, men en vanlig en er `gdb` for GNU/Linux\
  \ eller `lldb` for macOS."
title: "\xC5 bruke en debugger"
weight: 35
---

## Hvordan:
Rust støtter forskjellige debuggere, men en vanlig en er `gdb` for GNU/Linux eller `lldb` for macOS. Du kan også bruke `rust-gdb` eller `rust-lldb`, som er wrappere som pent skriver ut Rust-verdier. Her er et glimt:

```Rust
fn main() {
    let mut counter = 0;
    for _ in 0..5 {
        counter += 1;
        println!("Telleren er på: {}", counter);
    }
}
```

For å debugge dette, kompiler med debug-info:

```shell
$ rustc -g counter.rs
```

Kjør den så i `rust-gdb`:

```shell
$ rust-gdb counter
(gdb) break main
(gdb) run
(gdb) print counter
$1 = 0
(gdb) continue
Telleren er på: 1
(gdb) print counter
$2 = 1
```

## Dypdykk
Debugging har vært rundt siden *gamle dager* med hullkort, og utviklingen har vært en gave fra himmelen. Rust tilbyr sin egen verktøy med integreringer for GDB og LLDB på grunn av språkets systemnivå natur.

Alternativer for debugging av Rust-kode inkluderer bruk av integrerte utviklingsmiljøer (IDEer) med deres innebygde debuggere, som noen finner mer intuitive. Populære sånne inkluderer CLion med Rust-pluginen eller Visual Studio Code med Rust-utvidelsen.

Når det gjelder implementering, genererer Rust debug-symboler som disse debuggerne forstår, noe som er avgjørende for å trinnvis gå gjennom koden, sette breakpoints, og inspisere variabler uten å miste forstanden.

## Se Også
- The Rust Book om Debugging: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- Rust By Example om Feil og Debugging: https://doc.rust-lang.org/rust-by-example/error.html
- The Rust Language Server (RLS) som driver VS Code's Rust-utvidelse: https://github.com/rust-lang/rls
- Debugging Rust med Visual Studio Code: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
