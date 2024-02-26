---
date: 2024-01-26 04:10:23.734989-07:00
description: "\xC5 bruke en debugger er som \xE5 gi deg selv r\xF8ntgensyn for \xE5\
  \ titte inn i koden din under utf\xF8relse. Programmerere gj\xF8r dette for \xE5\
  \ oppdage feil, forst\xE5\u2026"
lastmod: '2024-02-25T18:49:38.759874-07:00'
model: gpt-4-0125-preview
summary: "\xC5 bruke en debugger er som \xE5 gi deg selv r\xF8ntgensyn for \xE5 titte\
  \ inn i koden din under utf\xF8relse. Programmerere gj\xF8r dette for \xE5 oppdage\
  \ feil, forst\xE5\u2026"
title: "\xC5 bruke en debugger"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å bruke en debugger er som å gi deg selv røntgensyn for å titte inn i koden din under utførelse. Programmerere gjør dette for å oppdage feil, forstå programflyten og sørge for at koden deres er så ren som fløyten. Det er som å ha en kompis som peker nøyaktig hvor du snublet.

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
