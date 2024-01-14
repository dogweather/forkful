---
title:                "Rust: Testien kirjoittaminen"
programming_language: "Rust"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/writing-tests.md"
---

{{< edit_this_page >}}

# Miksi kirjoittaa testejä Rust-ohjelmissa?

Testien kirjoittaminen on tärkeä osa ohjelmistojen kehittämistä ja parantamista kaikilla ohjelmointikielillä, mutta erityisesti Rustissa se on erityisen tärkeää. Rustin tyyppijärjestelmä ja lainvalvonta auttavat estämään monia ohjelmointivirheitä jo käännösaikana, mutta testsit tarjoavat lisävarmuuden ohjelman toiminnasta.

## Miten kirjoitetaan teste