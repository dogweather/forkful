---
date: 2024-01-20 17:35:46.343141-07:00
description: "La concatenazione di stringhe \xE8 il processo di unione di due o pi\xF9\
  \ stringhe in un'unica stringa. I programmatori la utilizzano per creare messaggi\u2026"
lastmod: '2024-03-13T22:44:43.209882-06:00'
model: gpt-4-1106-preview
summary: "La concatenazione di stringhe \xE8 il processo di unione di due o pi\xF9\
  \ stringhe in un'unica stringa."
title: Concatenazione di stringhe
weight: 3
---

## Come fare:
Ecco alcune vie per concatenare stringhe in Rust:

```Rust
fn main() {
    // Utilizzando l'operatore `+`
    let saluto = "Ciao".to_string();
    let mondo = "mondo!";
    let frase = saluto + " " + mondo;
    println!("{}", frase); // Output: Ciao mondo!
    
    // Utilizzando il macro `format!`
    let nome = "Alice";
    let saluto_personalizzato = format!("Ciao, {}!", nome);
    println!("{}", saluto_personalizzato); // Output: Ciao, Alice!
    
    // Utilizzando `push_str` su una String
    let mut messaggio = "Buongiorno".to_string();
    messaggio.push_str(", Italia!");
    println!("{}", messaggio); // Output: Buongiorno, Italia!
    
    // Concatenazione di più stringhe con `join`
    let parole = vec!["Rust", "è", "fantastico"];
    let frase = parole.join(" ");
    println!("{}", frase); // Output: Rust è fantastico
}
```

## Approfondimento
Concatenare stringhe non è sempre stato così semplice. In linguaggi più vecchi, spesso ci si doveva preoccupare della gestione della memoria e dell'allocazione. Rust gestisce automaticamente la memoria e garantisce la sicurezza nei tipi durante la concatenazione, prevenendo molti errori comuni.

In alternativa all'uso dei metodi sopra, si potrebbe lavorare con stringhe "pigre" come `Cow` (Copy-on-Write) per evitare alcune allocazioni quando possibile. Rust usa `String` per stringhe modificabili e `&str` per stringhe con dati prestabiliti o non modificabili.

Nella concatenazione c'è una sottigliezza: `+` prende il possesso della prima stringa e la appende alla seconda. Questo significa che non puoi più usare la prima stringa dopo la concatenazione. Inoltre, solo il lato destro di `+` può essere una referenza; il metodo `format!` è più flessibile, ma può essere meno efficiente a causa delle allocazioni di memoria extra che potrebbero avvenire.

## Vedi anche
- Rust Book su stringhe: https://doc.rust-lang.org/book/ch08-02-strings.html
- Rust by Example su stringhe: https://doc.rust-lang.org/rust-by-example/std/str.html
- Rust std::string::String documentation: https://doc.rust-lang.org/std/string/struct.String.html
