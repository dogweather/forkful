---
title:                "Rust: Å finne lengden på en snor."
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Å finne lengden på en streng er en grunnleggende operasjon i mange programmeringsspråk. I Rust, kan dette gjøres på flere måter avhengig av behovene til utvikleren.

## Hvordan
```Rust
let string = "Hei, verden!";
let len = string.len();

// Output: len = 12
```
Den enkleste måten å finne lengden på en streng i Rust er å bruke `.len()` funksjonen. Denne funksjonen returnerer lengden på strengen som et heltall.

Hvis du ønsker å finne lengden på en streng som inneholder ikke-ASCII-tekst, som for eksempel Unicode-tegn, kan du bruke `.chars()` funksjonen sammen med `.count()` funksjonen.

```Rust
let string = "こんにちは、世界！";
let len = string.chars().count();

// Output: len = 8
```
I dette eksemplet, blir lengden på strengen beregnet ved å først dele den opp i en samling av tegn, og deretter telle antall tegn i samlingen.

## Deep Dive
For å virkelig forstå hvordan lengden på en streng blir funnet i Rust, må vi gå litt ned i dybden. I Rust, er strenger representert som UTF-8 sekvenser. Dette betyr at hver karakter kan bestå av flere bytes. Når vi bruker `.len()` funksjonen, blir antall bytes i strengen returnert.

Når det gjelder å finne lengden på en streng med Unicode-tegn, er `.chars()` og `.count()` funksjonene nyttige fordi de tar hensyn til at noen tegn kan bestå av flere bytes. Dette er en viktig ting å huske på når man arbeider med strenger i Rust.

## Se også
- [Offisiell Rust Dokumentasjon for Strenger](https://doc.rust-lang.org/std/string/)
- [Rust By Examples: Strenger](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- [Følger oppskrift for strenger i Rust](https://ovid.github.io/rust/guide/2016/02/03/strings.html)