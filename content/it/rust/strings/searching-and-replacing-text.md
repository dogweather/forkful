---
date: 2024-01-20 17:59:05.376180-07:00
description: "Cercare e sostituire del testo \xE8 l'atto di trovare specifiche stringhe\
  \ in un dato testo e scambiarle con altre. I programmatori lo fanno per correzioni\
  \ in\u2026"
lastmod: '2024-03-13T22:44:43.203373-06:00'
model: gpt-4-1106-preview
summary: "Cercare e sostituire del testo \xE8 l'atto di trovare specifiche stringhe\
  \ in un dato testo e scambiarle con altre."
title: Ricerca e sostituzione del testo
weight: 10
---

## How to (Come fare):
In Rust, puoi usare il metodo `replace` per stringhe. Ecco un esempio:

```Rust
fn main() {
    let testo = "Ciao mondo!";
    let testo_sostituito = testo.replace("mondo", "Rust");
    println!("{}", testo_sostituito);  // Stampa "Ciao Rust!"
}
```

Per sostituzioni più complesse puoi usare le espressioni regolari con il crate `regex`:

```Rust
use regex::Regex;

fn main() {
    let testo = "Ciao mondo! Rust è fantastico.";
    let regex = Regex::new(r"\b[Rr]ust\b").unwrap();
    let risultato = regex.replace_all(&testo, "JavaScript");
    
    println!("{}", risultato);  // Stampa "Ciao mondo! JavaScript è fantastico."
}
```

Output:
```
Ciao Rust!
Ciao mondo! JavaScript è fantastico.
```

## Deep Dive (Approfondimento):
La sostituzione di testo è un'operazione comune da quando esistono i computer, con radici nelle prime fasi dell'editing testuale. In altri linguaggi, come Python o JavaScript, abbiamo funzionalità simili. Rust offre prestazioni superiori grazie alla sua gestione della memoria e alla sicurezza dei tipi.

I crate, come `regex`, permettono sostituzioni basate su pattern flessibili. Le espressioni regolari sono un linguaggio a sé che permette di trovare corrispondenze complesse con una sintassi concisa.

Oltre a `replace` e `regex`, ci sono crate come `strsim` per algoritmi di confronto tra stringhe e `aho-corasick` per ricerche multiple ed efficienti.

Implementare la ricerca e la sostituzione da zero in Rust richiede una comprensione dell'ownership, delle lifetimes, e della gestione safe dell'accesso ai dati, soprattutto quando lavoriamo con prestiti mutabili o immutabili.

## See Also (Vedi Anche):
- [La documentazione ufficiale di Rust su `String`](https://doc.rust-lang.org/std/string/struct.String.html)
- [Differenze tra le stringhe in Rust: `String` e `&str`](https://doc.rust-lang.org/book/ch04-03-slices.html#string-slices)
- [Un'introduzione alle espressioni regolari](https://docs.rs/regex/1.3.9/regex/#syntax)
