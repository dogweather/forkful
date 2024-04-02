---
date: 2024-01-20 17:46:36.200626-07:00
description: "Estrarre sottocordi in Rust significa prendere pezzi di testo da una\
  \ stringa. Facciamo questo per manipolare, analizzare o semplicemente visualizzare\
  \ solo\u2026"
lastmod: '2024-03-13T22:44:43.207151-06:00'
model: gpt-4-1106-preview
summary: "Estrarre sottocordi in Rust significa prendere pezzi di testo da una stringa.\
  \ Facciamo questo per manipolare, analizzare o semplicemente visualizzare solo\u2026"
title: Estrazione di sottostringhe
weight: 6
---

## What & Why?
Estrarre sottocordi in Rust significa prendere pezzi di testo da una stringa. Facciamo questo per manipolare, analizzare o semplicemente visualizzare solo le parti che ci servono.

## How to:
```Rust
fn main() {
    let frase = "Rust è fantastico!";
    let start = 5;
    let end = 14;

    // Estrai sottocorda utilizzando slicing
    let sottocorda = &frase[start..end];
    println!("Estratto: {}", sottocorda); // Stampa "è fantast"
}
```

Se esegui questo codice, il tuo output sarà:
```
Estratto: è fantast
```

Ricorda, gli indici in Rust iniziano da 0. Fai attenzione a non andare fuori dai limiti!

## Deep Dive
Prima dell'avvento di linguaggi sicuri come Rust, estrarre sottocordi poteva essere una faccenda rischiosa - errori come buffer overflow erano comuni. Rust previene questi problemi attraverso il controllo dei prestiti e la verifica degli indici a compile time.

In Rust, puoi anche usare la libreria standard `str::find` per trovare indici o usare crate come `regex` per pattern complessi. Slicing diretto è veloce e sicuro, ma devi sapere gli indici di partenza e di fine.

Quando fai il slicing, crei un "prestito" &str della stringa originale senza allocare nuova memoria. È efficiente ma attenzione: se modifichi la stringa originale, il tuo sottocorda potrebbe non essere più valido.

## See Also
- Documentazione Rust sulle stringhe: https://doc.rust-lang.org/std/string/index.html
- Rust Book sui dati di testo `str` e `String`: https://doc.rust-lang.org/book/ch08-02-strings.html
- Crate `regex` per espressioni regolari: https://crates.io/crates/regex

Non fermarti qui. Esercitati e scopri tutti i modi in cui puoi manipolare le stringhe in Rust. Buona codifica!
