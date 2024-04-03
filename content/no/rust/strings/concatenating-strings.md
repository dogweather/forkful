---
date: 2024-01-20 17:35:32.634778-07:00
description: "I programmering betyr strengkonkatenering \xE5 sette sammen to eller\
  \ flere tekststykker til ett. Vi gj\xF8r det for \xE5 bygge setninger, lage dynamiske\
  \ meldinger,\u2026"
lastmod: '2024-03-13T22:44:40.565909-06:00'
model: gpt-4-1106-preview
summary: "I programmering betyr strengkonkatenering \xE5 sette sammen to eller flere\
  \ tekststykker til ett."
title: "Sammensl\xE5ing av strenger"
weight: 3
---

## Hvordan:
I Rust brukes `+`-operatoren eller `format!`-makroen for å slå sammen strenger. Her er noen eksempler:

```Rust
fn main() {
    // Concatenate with `+`
    let hello = "Hei".to_string();
    let world = "verden!";
    let hello_world = hello + " " + world;
    println!("{}", hello_world); // Output: Hei verden!

    // Concatenate using `format!`
    let name = "Ola";
    let greeting = format!("{} {}", hello_world, name);
    println!("{}", greeting); // Output: Hei verden! Ola
}
```

## Dypdykk
I de tidlige dagene av programmering, før Rust, var strengkonkatenering ofte kompleks og feilutsatt. Rust innførte eierskapsmodellen som gjør strengkonkatenering litt annerledes, men sikrere og mer forutsigbar. For eksempel, ved å bruke `+`, overføres eierskapet, og den første strengen kan ikke gjenbrukes uten å bli klonet først. Alternativer til `+` inkluderer `format!`, som er mer fleksibel og lett å bruke når man kombinerer flere verdier av forskjellige typer, eller metodene `push` og `push_str` for `String`.

Når det gjelder implementasjon, lagrer Rust strenger som `String` type, som representerer en voksende, endringsdyktig tekst sekvens. Bak kulissene bruker `String` en `Vec<u8>` for å holde på dataen, noe som betyr at den er lagret i UTF-8 format.

## Se også:
- [The Rust Programming Language, Ch 08-02: "Storing UTF-8 Encoded Text with Strings"](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Rust Documentation on `std::string::String`](https://doc.rust-lang.org/std/string/struct.String.html)
