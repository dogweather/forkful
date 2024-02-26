---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:19.510539-07:00
description: "Het lezen van commandoregelargumenten in Rust stelt programma's in staat\
  \ om gebruikersinvoer bij opstart te nemen. Het is essentieel voor aangepast gedrag\u2026"
lastmod: '2024-02-25T18:49:47.953576-07:00'
model: gpt-4-0125-preview
summary: "Het lezen van commandoregelargumenten in Rust stelt programma's in staat\
  \ om gebruikersinvoer bij opstart te nemen. Het is essentieel voor aangepast gedrag\u2026"
title: Commandoregelargumenten lezen
---

{{< edit_this_page >}}

## Wat & Waarom?

Het lezen van commandoregelargumenten in Rust stelt programma's in staat om gebruikersinvoer bij opstart te nemen. Het is essentieel voor aangepast gedrag zonder een grafische gebruikersinterface (GUI).

## Hoe te:

Hier is de eenvoudigste manier om argumenten te grijpen:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
}
```

Voer het uit met `cargo run arg1 arg2`. Je zult zien:

```
["pad/naar/uitvoerbaar", "arg1", "arg2"]
```

Een nettere optie met iterators:

```Rust
use std::env;

fn main() {
    for arg in env::args().skip(1) {
        println!("{}", arg);
    }
}
```

Probeer nu `cargo run cool stuff`:

```
cool
stuff
```

## Diepgaande duik

Historisch gezien zijn commandoregelargumenten een terugblik naar de dagen dat GUI's niet wijdverspreid waren. Nu zijn ze geweldig voor scripts, servers of hulpprogramma's.

Rust's `std::env::args` gebruikt een iterator, wat geheugenefficiënt en lui is. Het kan ook Unicode aan. Er is ook `args_os` voor ruwe OS-strings.

Voor complexe parsing zijn crates zoals `clap` of `structopt` handig. Ze parsen vlaggen, opties en subcommando's.

## Zie ook

- [De Rust `std::env` module](https://doc.rust-lang.org/std/env/)
- [`clap` crate documentatie](https://docs.rs/clap/)
- [Het Rust Boek over Commandoregelargumenten](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
