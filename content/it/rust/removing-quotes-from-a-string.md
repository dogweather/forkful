---
title:                "Rimuovere le virgolette da una stringa"
date:                  2024-01-26T03:41:31.811737-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rimuovere le virgolette da una stringa"

category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Rimuovere le virgolette da una stringa in Rust significa eliminare caratteri di virgolette extra non necessari che potrebbero essere avvolti attorno ai tuoi dati di testo. I programmatori fanno ciò quando hanno bisogno di pulire o normalizzare le stringhe, magari dopo il parsing dei dati da un file, o quando li stanno preparando per un altro formato in cui le virgolette potrebbero essere problematiche o ridondanti.

## Come fare:

```Rust
fn remove_quotes(s: &str) -> String {
    s.trim_matches(|c| c == '\"' || c == '\'').to_string()
}

fn main() {
    let quoted_str = "\"Ciao, Rustacei!\"";
    let cleaned_str = remove_quotes(quoted_str);
    println!("{}", cleaned_str);
    // Output: Ciao, Rustacei!
}
```

A volte puoi avere una stringa con virgolette miste, come questa:

```Rust
fn main() {
    let mixed_quoted = "'Rust dice: \"Ciao, Mondo!\"'";
    let cleaned_str = remove_quotes(mixed_quoted);
    println!("{}", cleaned_str);
    // Output: Rust dice: "Ciao, Mondo!"
}
```

Qui, vengono rimossi solo le virgolette singole più esterne.

## Approfondimento

Quando si rimuovono le virgolette da una stringa, potresti chiederti perché non si fa semplicemente un `.replace("\"", "")`. In passato, la gestione del testo era meno standardizzata e diversi sistemi avevano modi diversi di memorizzare e trasmettere testo, spesso con qualche tipo di 'sequenza di escape' per caratteri speciali. Il metodo `trim_matches` di Rust è più versatile, permettendo di specificare più caratteri da tagliare, e se tagliare dall'inizio (prefisso), dalla fine (suffisso), o da entrambi i lati della stringa.

Ci sono alternative, ovviamente. Regex è il gigante per la manipolazione delle stringhe, capace di corrispondere a pattern complessi, e sarebbe eccessivo solo per rimuovere le virgolette. Librerie come `trim_in_place` potrebbero offrire un taglio sul posto senza il sovraccarico di creare un nuovo oggetto `String`, che potrebbe essere desiderabile per applicazioni critiche per le prestazioni.

Sotto il cofano, `trim_matches` in realtà itera attraverso i caratteri della stringa da entrambe le estremità, controllando rispetto al pattern fornito fino a quando non viene trovato un carattere non corrispondente. È efficiente per quello che fa, ma sii sempre consapevole che lavora con valori scalari Unicode. Se la tua stringa potrebbe contenere caratteri Unicode multi-byte, non devi preoccuparti che li spezzi.

## Vedi Anche

- Documentazione di Rust sulla manipolazione delle stringhe: https://doc.rust-lang.org/book/ch08-02-strings.html
- Il crate `regex` per pattern complessi: https://crates.io/crates/regex
- Rust by Example per scenari pratici di codifica: https://doc.rust-lang.org/stable/rust-by-example/std/str.html
