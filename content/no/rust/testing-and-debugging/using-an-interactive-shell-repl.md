---
date: 2024-01-26 04:18:02.984868-07:00
description: "Et Rust-interaktivt skall, eller REPL (Read-Eval-Print Loop), lar deg\
  \ kj\xF8re Rust-kode p\xE5 sparket, og se umiddelbare resultater, perfekt for\u2026"
lastmod: '2024-03-13T22:44:40.575356-06:00'
model: gpt-4-0125-preview
summary: "Et Rust-interaktivt skall, eller REPL (Read-Eval-Print Loop), lar deg kj\xF8\
  re Rust-kode p\xE5 sparket, og se umiddelbare resultater, perfekt for eksperimentering\
  \ eller l\xE6ring."
title: Bruke et interaktivt skall (REPL)
weight: 34
---

## Hva & Hvorfor?
Et Rust-interaktivt skall, eller REPL (Read-Eval-Print Loop), lar deg kjøre Rust-kode på sparket, og se umiddelbare resultater, perfekt for eksperimentering eller læring. Programmerere bruker det til å teste kodebiter, feilsøke, eller bare leke med språkfunksjoner uten overhead av å kompilere et helt prosjekt.

## Hvordan:
Per nå har ikke Rust en offisiell REPL inkludert. Du kan bruke tredjepartsverktøy som `evcxr_repl`. Installer det med Cargo:

```sh
cargo install evcxr_repl
```

Deretter, kjør REPL:

```sh
evcxr
```

Inne, test noe Rust-kode:

```rust
let x = 5;
let y = 3;
println!("{} + {} = {}", x, y, x + y);
```

Output bør være:

```
5 + 3 = 8
```

## Dypdykk
Rusts ethos er sentrert rundt sikkerhet og ytelse, som vanligvis er assosiert med språk som kompileres på forhånd, og mindre med tolkede, REPL-vennlige språk. Historisk sett har språk som Python eller Ruby prioritert å ha en REPL for umiddelbar tilbakemelding, men var ikke designet med systemnivå oppgaver i tankene.

Til tross for fraværet av en offisiell REPL i Rust, har et par alternativer som `evcxr_repl` dukket opp. Disse prosjektene er ikke bare å hacke Rust inn i en REPL; de fletter smart sammen språkets kompiler-og-kjør syklus inn i en interaktiv økt. REPL-en kompilerer koden i bakgrunnen og kjører binærfilen, og fanger opp output. På denne måten bevarer den Rusts ytelsesfordeler samtidig som den gir den interaktive opplevelsen.

Det pågår en løpende diskusjon i Rust-fellesskapet om offisiell REPL-støtte, og med hver språkitereasjon ser vi mer verktøysfinesse som til slutt kan føre til en innfødt løsning.

## Se Også
For mer info og andre verktøy:
- Evcxr REPL GitHub-repo: [https://github.com/google/evcxr](https://github.com/google/evcxr)
- Rust Playground, en nettbasert måte å teste Rust-kode på: [https://play.rust-lang.org/](https://play.rust-lang.org/)
- Rust Language-diskusjon om REPL-funksjon: [https://internals.rust-lang.org/](https://internals.rust-lang.org/)
