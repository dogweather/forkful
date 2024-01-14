---
title:    "Rust: Eliminazione dei caratteri corrispondenti a un modello."
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Perché

Cancellare i caratteri corrispondenti a un determinato modello può sembrare un'operazione inutile, ma in realtà può essere molto utile per pulire i dati o per migliorare il processo di analisi dei dati in un progetto Rust.

# Come fare

Per cancellare i caratteri che corrispondono a un pattern in Rust, possiamo utilizzare il metodo `trim_matches()` della libreria standard. Ad esempio, se vogliamo rimuovere tutti i caratteri `x` da una stringa, possiamo utilizzare il seguente codice:

```Rust
let stringa = "ciaoxxxmondo";
let stringa_pulita = stringa.trim_matches('x');
println!("{}", stringa_pulita); //stamperà "ciaomondo"
```

In questo esempio, il carattere `x` viene rimosso dalla stringa e il risultato viene assegnato alla variabile `stringa_pulita`. Successivamente viene stampata la stringa pulita, senza i caratteri eliminati.

# Approfondimento

Il metodo `trim_matches()` accetta un parametro di tipo `char` che corrisponde al carattere o al pattern che vogliamo eliminare dalla stringa. È possibile utilizzare anche più di un carattere all'interno delle parentesi per eliminare più caratteri corrispondenti. Ad esempio, se volessimo eliminare sia la lettera `x` che la lettera `y` dalla stringa, potremmo scrivere il seguente codice:

```Rust
let stringa = "ciaoxxxmondo";
let stringa_pulita = stringa.trim_matches(['x', 'y']);
println!("{}", stringa_pulita); //stamperà "ciaomondo"
```

In questo caso, viene restituita una stringa senza i caratteri `x` e `y` eliminati.

# Vedi anche

- [Documentazione del metodo `trim_matches()`](https://doc.rust-lang.org/std/primitive.str.html#method.trim_matches)
- [Esempi di utilizzo di `trim_matches()`](https://github.com/rust-lang/rust/issues/15015)