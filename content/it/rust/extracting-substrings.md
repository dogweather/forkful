---
title:                "Estrazione di sottostringhe"
html_title:           "Rust: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore in Rust, potresti trovarti nella situazione in cui devi lavorare con stringhe di testo e hai bisogno di estrarre parti specifiche di queste stringhe. In situazioni come queste, sapere come estrarre sottotestuali è fondamentale per scrivere codice efficiente ed efficace.

## Come Fare

Per estrarre una sottotestuale da una stringa in Rust, possiamo usare il metodo `.get()` seguito dall'indice della substring che vogliamo ottenere. Ad esempio, se vogliamo estrarre la prima sottotestuale di una stringa, possiamo usare il seguente codice:

```Rust
let stringa = "Ciao mondo!";
let primo_carattere = stringa.get(0);
println!("La prima sottotestuale è: {:?}", primo_carattere); // output: La prima sottotestuale è: Some('C')
```

Se vogliamo estrarre più di una sottotestuale, possiamo usare il metodo `.get()` con un intervallo di indici, che restituirà un riferimento alla sottotestuale specificata. Ad esempio:

```Rust
let stringa = "Ciao mondo!";
let primo_trio_caratteri = stringa.get(0..3);
println!("I primi tre caratteri sono: {:?}", primo_trio_caratteri); // output: I primi tre caratteri sono: Some("Cia")
```

È importante notare che il metodo `.get()` restituisce un `Option` che può essere `Some` se la sottotestuale è stata trovata o `None` se non è stata trovata.

## Approfondimento

Il metodo `.get()` è solo uno dei modi per estrarre sottotestuali in Rust. Possiamo anche utilizzare il metodo `.as_str()` per ottenere una referenza alla stringa originale, o il metodo `.chars()` per ottenere un iteratore sui caratteri della stringa. Inoltre, possiamo usare gli indici unicode per estrarre sottotestuali di stringhe in UTF-8.

## Vedi Anche

- [Documentazione di Rust sulle stringhe](https://doc.rust-lang.org/std/string/index.html)
- [Tutorial di programmazione Rust](https://www.rust-lang.org/learn)
- [Rust Playground](https://play.rust-lang.org/)